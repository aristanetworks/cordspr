// Write is needed by Command::Stdin.write_all()
use std::io::Write;
use std::process::{Command, Stdio};

use crate::{
    error::{Error, Result},
};

// Notice: use BTreeMap to make it easier to iterate trailer keys in order.
pub type TrailerMap = std::collections::BTreeMap<String, Vec<String>>;

#[derive(Debug, PartialEq)]
pub struct CommitMessage {

    /// Subject of the message (i.e. very first line)
    pub subject:  String,

    /// Body of the message, *EXCLUDING* the contents of the trailers
    /// section. Empty string if no body.
    pub body:     String,

    /// Map of trailer keys to trailer values (e.g, "key: value...").
    pub trailers: TrailerMap,
}

impl CommitMessage {

    pub fn render(&self) -> String {
        let mut ret: String = "".to_string();

        if self.subject.len() == 0 {
            ret.push_str("MISSING COMMIT MESSAGE SUBJECT!\n");
        } else {
            ret.push_str(&format!("{}\n", &self.subject));
        }

        if self.body.len() > 0 {
           ret.push_str(&format!("\n{}\n", &self.body));
        }

        if self.trailers.len() > 0 {
            ret.push_str("\n");
            for (k, vec) in self.trailers.iter() {
                for v in vec.iter() {
                    ret.push_str(&format!("{k}: {v}\n"));
                }
            }
        }

        ret
    }
}

/// Parse the contents of a git commit message into a CommitMessage instance.
pub fn parse_commit_message(
    orig_msg: &str,
) -> Result<CommitMessage> {

    // Get rid of trailing empty/blank lines and replace all CRLFs with
    // just LFs upfront to simplify parsing logic.
    let msg: &str = &orig_msg
        .trim_end()
        .replace("\r\n", "\n");

    // Parse trailers using the 'git interpret-trailers --parse` command
    // into a trailer map.
    let trailers = parse_trailers(msg)?;

    // Use 1st line as the message subject and the rest as the first version
    // of the body. The trailers paragraph, if present, will be later removed
    // from the body.
    let v: Vec<&str> = msg.splitn(2, "\n").collect();
    let subject: String = v[0].to_string();

    let mut body: String = "".to_string();
    if v.len() > 1 {
        body = v[1].to_string();
    }

    // Add back the \n to the beginning of the body so that we can look for
    // "\n\n" when searching for the trailers paragraph.
    body.insert(0, '\n');

    // If there are trailers, remove the "trailers paragraph" from the bottom
    // of the body. The trailers paragraph is the last block of t
    if trailers.len() > 0 {
        let v: Vec<&str> = body.rsplitn(2, "\n\n").collect();
        if v.len() > 1 {
            // rsplitn() gets the split parts in reverse order, i.e. last part
            // first, so we need to use v[1] to get the body.
            body = v[1].to_string();
        }
    }

    // Remove body's heading/trailing empty/blank lines.
    body = body.trim().to_string();

    Ok(CommitMessage {
        subject: subject,
        body: body,
        trailers: trailers,
    })
}

/// Parse the commit message trailers using 'git interpret-trailers --parse'
///
/// This is the "authoritative" way to parse trailers.
///
/// This function pipes the provided `msg` into the stdin of the 'git
/// interpret-trailers --parse' command and returns the parsed contents.
///
/// Notice that returned contents might be different from what is the trailer
/// section of `msg`. For example, multi-line trailers are flattened. Example:
///
///  Foo: foo
///    plus more foo here
///
/// Is returned as:
///
///  Foo: foo plus more foo here
///
fn parse_raw_trailers(
    msg: &str,
) -> Result<String> {

    let mut child = Command::new("git")
        .arg("interpret-trailers")
        .arg("--parse")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let child_stdin = child.stdin.as_mut().unwrap();
    child_stdin.write_all(msg.trim_end().as_bytes())?;

    // Close stdin to finish and avoid indefinite blocking
    drop(child_stdin);

    let output = child.wait_with_output()?;

    if !output.status.success() {
        return Err(Error::new(
            format!("error executing 'git interpret-trailers': {}",
                    std::str::from_utf8(&output.stdout).unwrap()),
        ));
    }

    let stdout = std::str::from_utf8(&output.stdout).unwrap().to_string();

    Ok(stdout)
}

fn parse_trailers(
     msg: &str,
) -> Result<TrailerMap> {

    // Parse trailers using the 'git interpret-trailers --parse` command
    // and convert the results into a trailer map.
    let raw_trailers = parse_raw_trailers(msg.trim_end())?;

    let regex = lazy_regex::regex!(r#"([\ws\s-]+?):\s*(.*)$"#);

    let mut trailers = TrailerMap::new();

    for line in raw_trailers
        .trim()
        .split('\n')
        .map(|line| line.trim_end())
    {
        if let Some(caps) = regex.captures(line) {
            let k = caps.get(1).unwrap().as_str().to_string();
            let v = caps.get(2).unwrap().as_str().to_string();

            if let Some(vec) = trailers.get_mut(&k) {
                vec.push(v.clone())
            } else {
                trailers.insert(k.clone(), vec![v.clone()]);
            }
        }
    }

    Ok(trailers)
}

// =====================================================================
// tests

#[cfg(test)]
mod test {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    fn s(s: &str) -> String {
        s.to_string()
    }

    // -------------------------------------------------
    // parse_commit_message() tests

    fn must_parse(msg: &str) -> CommitMessage {
        let cm = parse_commit_message(msg);
        assert!(cm.is_ok(), "commit message parse error: msg={:?} error={:?}", msg, cm);
        cm.unwrap()
    }

    #[test]
    fn test_parse_just_subject() {
        assert_eq!(
            must_parse("Just subject"),
            CommitMessage {
                subject: s("Just subject"),
                body: s(""),
                trailers: TrailerMap::new(),
            },
        );

        assert_eq!(
            must_parse("Just subject with newline\n"),
            CommitMessage {
                subject: s("Just subject with newline"),
                body: s(""),
                trailers: TrailerMap::new(),
            },
        );
    }

    #[test]
    fn test_parse_no_newline_before_body() {
        assert_eq!(
            must_parse("No newline before body\nThe body"),
            CommitMessage {
                subject: s("No newline before body"),
                body: s("The body"),
                trailers: TrailerMap::new(),
            },
        );
    }

    #[test]
    fn test_parse_subject_and_body() {
        assert_eq!(
            must_parse("Subject and body\n\nThe body\nparts"),
            CommitMessage {
                subject: s("Subject and body"),
                body: s("The body\nparts"),
                trailers: TrailerMap::new(),
            },
        );
    }

    #[test]
    fn test_parse_subject_and_body_with_paragraphs() {
        assert_eq!(
            must_parse(r#"Body with paragraphs

Paragraph1
ends here.

Paragraph2
ends here.

Paragraph3
ends here.


"#),
            CommitMessage {
                subject: s("Body with paragraphs"),
                body: s(r#"Paragraph1
ends here.

Paragraph2
ends here.

Paragraph3
ends here."#),
                trailers: TrailerMap::new(),
            },
        );
    }

    #[test]
    fn test_parse_single_line_trailers() {
        assert_eq!(
            must_parse(r#"Single line trailers

Paragraph1
ends here.

Foo: FOO1    FOO2    
Bar:     BAR1 BAR2

"#),
            CommitMessage {
                subject: s("Single line trailers"),
                body: s("Paragraph1\nends here."),
                trailers: TrailerMap::from( [
                    ( s("Foo"), vec![ s("FOO1    FOO2") ] ),
                    ( s("Bar"), vec![ s("BAR1 BAR2") ] ),
                ] ),
            },
        );
    }

    #[test]
    fn test_parse_multi_line_trailers() {
        assert_eq!(
            must_parse(r#"Multi-line trailers

Body with list:

- foo
- bar
- baz

Foo: FOO1
  FOO2
Bar:     BAR1
  BAR2

"#),
            CommitMessage {
                subject: s("Multi-line trailers"),
                body: s("Body with list:\n\n- foo\n- bar\n- baz"),
                trailers: TrailerMap::from( [
                    ( s("Foo"), vec![ s("FOO1 FOO2") ] ),
                    ( s("Bar"), vec![ s("BAR1 BAR2") ] ),
                ] ),
            },
        );
    }

    #[test]
    fn test_parse_multiple_trailer_entries() {
        assert_eq!(
            must_parse(r#"Multiple trailer entries

The body.

Foo: FOO1
Bar: BAR1 BAR2
Foo: FOO2 FOO3
Bar: BAR3

"#),
            CommitMessage {
                subject: s("Multiple trailer entries"),
                body: s("The body."),
                trailers: TrailerMap::from( [
                    ( s("Foo"), vec![ s("FOO1"), s("FOO2 FOO3") ] ),
                    ( s("Bar"), vec![ s("BAR1 BAR2"), s("BAR3") ] ),
                ] ),
            },
        );
    }

    #[test]
    fn test_parse_with_incorrectly_placed_trailer() {
        assert_eq!(
            must_parse(r#"Incorrectly placed trailer

The body.

Incorrectly-placed-trailer: value

Foo: FOO1
Bar: BAR1 BAR2
Foo: FOO2 FOO3
Bar: BAR3

"#),
            CommitMessage {
                subject: s("Incorrectly placed trailer"),
                body: s(r#"The body.

Incorrectly-placed-trailer: value"#),
                trailers: TrailerMap::from( [
                    ( s("Foo"), vec![ s("FOO1"), s("FOO2 FOO3") ] ),
                    ( s("Bar"), vec![ s("BAR1 BAR2"), s("BAR3") ] ),
                ] ),
            },
        );
    }

    // -------------------------------------------------
    // CommitMessage.render() tests

    #[test]
    fn test_render_missing_subject() {
        assert_eq!(
            CommitMessage {
                subject: s(""),
                body: s(""),
                trailers: TrailerMap::new(),
            }.render(),
            "MISSING COMMIT MESSAGE SUBJECT!\n",
        );
    }

    #[test]
    fn test_render_just_subject() {
        assert_eq!(
            CommitMessage {
                subject: s("Just subject"),
                body: s(""),
                trailers: TrailerMap::new(),
            }.render(),
            "Just subject\n",
        );
    }

    #[test]
    fn test_render_subject_and_body() {
        assert_eq!(
            CommitMessage {
                subject: s("Subject and body"),
                body: s("The body\nparts"),
                trailers: TrailerMap::new(),
            }.render(),
            "Subject and body\n\nThe body\nparts\n",
        );
    }

    #[test]
    fn test_render_subject_and_trailers() {
        assert_eq!(
            CommitMessage {
                subject: s("Subject and trailers"),
                body: s(""),
                trailers: TrailerMap::from( [
                    ( s("Foo"), vec![ s("FOO1") ] ),
                    ( s("Bar"), vec![ s("BAR1") ] ),
                ] ),
            }.render(),
            r#"Subject and trailers

Bar: BAR1
Foo: FOO1
"#,
        );
    }

    #[test]
    fn test_render_subject_body_and_trailers() {
        assert_eq!(
            CommitMessage {
                subject: s("Subject, body and trailers"),
                body: s("Paragraph1\nends here.\n\nParagraph2\nends here."),
                trailers: TrailerMap::from( [
                    ( s("Foo"), vec![ s("FOO1"), s("FOO2 FOO3") ] ),
                    ( s("Bar"), vec![ s("BAR1 BAR2"), s("BAR3") ] ),
                ] ),
            }.render(),
            r#"Subject, body and trailers

Paragraph1
ends here.

Paragraph2
ends here.

Bar: BAR1 BAR2
Bar: BAR3
Foo: FOO1
Foo: FOO2 FOO3
"#,
        );
    }
}
