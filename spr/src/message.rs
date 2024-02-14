/*
 * Copyright (c) Radical HQ Limited
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{
    error::{Error, Result},
    output::output,
    commit_message::parse_commit_message,
};

pub type MessageSectionsMap =
    std::collections::BTreeMap<MessageSection, String>;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, EnumIter)]
pub enum MessageSection {
    Title,
    Summary,
    TestPlan,
    Reviewers,
    ReviewedBy,
    PullRequest,
    // NOTICE: ExtraTrailers is not a real section found in messages,
    // but just a mechanism to store the real trailers that are not known
    // to spr.
    ExtraTrailers,
}

pub fn message_section_label(section: &MessageSection) -> &'static str {
    use MessageSection::*;

    // Temporary remedial adjustments to be somewhat compatible with git trailers

    // match section {
    //     Title => "Title",
    //     Summary => "Summary",
    //     TestPlan => "Test Plan",
    //     Reviewers => "Reviewers",
    //     ReviewedBy => "Reviewed By",
    //     PullRequest => "Pull Request",
    // }
    match section {
        Title => "Title",
        Summary => "Summary",
        TestPlan => "Test-Plan",
        Reviewers => "Reviewers",
        ReviewedBy => "Reviewed-By",
        PullRequest => "Pull-Request",
        ExtraTrailers => "__EXTRA_TRAILERS_IS_NOT_A_REAL_SECTION__",
    }
}

pub fn message_section_by_label(label: &str) -> Option<MessageSection> {
    use MessageSection::*;

    // Temporary remedial adjustments to be somewhat compatible with git trailers

    // match &label.to_ascii_lowercase()[..] {
    //     "title" => Some(Title),
    //     "summary" => Some(Summary),
    //     "test plan" => Some(TestPlan),
    //     "reviewer" => Some(Reviewers),
    //     "reviewers" => Some(Reviewers),
    //     "reviewed by" => Some(ReviewedBy),
    //     "pull request" => Some(PullRequest),
    //     _ => None,
    // }
    match label {
        "Title" => Some(Title),
        "Summary" => Some(Summary),
        "Test-Plan" => Some(TestPlan),
        "Reviewer" => Some(Reviewers),
        "Reviewers" => Some(Reviewers),
        "Reviewed-By" => Some(ReviewedBy),
        "Pull-Request" => Some(PullRequest),
        // NOTICE: don't match ExtraTrailers, as it's not a real section.
        _ => None,
    }
}

fn message_section_is_trailer(section: &MessageSection) -> bool {
    use MessageSection::*;

    match section {
        Title => false,
        Summary => false,
        // NOTICE: even though ExtraTrailers *contains* trailers, it's
        // not a trailer itself.
        ExtraTrailers => false,
        _ => true,
    }
}

pub fn parse_message(
    orig_msg: &str,
    top_section: MessageSection,
) -> Result<MessageSectionsMap> {

    let msg = orig_msg.trim();

    let mut sections = MessageSectionsMap::new();

    // Parse the commit message and populate the sections map based on
    // what was required. First, the title and summary.
    let cmsg = parse_commit_message(msg)?;

    if top_section == MessageSection::Title {
        sections.insert(MessageSection::Title, cmsg.subject);
    }

    if top_section <= MessageSection::Summary && cmsg.body.len() > 0 {
        sections.insert(MessageSection::Summary, cmsg.body);
    }

    // Now look for the all requested section names in the trailer map.
    for section in MessageSection::iter() {
        if section < top_section || !message_section_is_trailer(&section) {
            continue;
        }

        let label = message_section_label(&section);
        if let Some(vec) = cmsg.trailers.get(label) {
            let text = vec.join(" ");
            sections.insert(section, text);
        }
    }

    // Now, store the *rendered* contents of all trailers that are not
    // known section names in the special ExtraTrailers "section".
    //
    // Notice that this is different that the other sections, where they
    // map the section name to the section contents. In the "ExtraTrailers"
    // section, we store multiple trailers in already rendered form, e.g.:
    //
    //    "Reviewers"          => "john, mary"
    //    "TestPlan"           => "http://example.com/my_plan"
    //    "__EXTRA_TRAILERS__" => "Foo: bar\nBaz: buz\nBlah: Bleh"
    //
    let mut extra_trailers = String::new();
    if cmsg.trailers.len() > 0 {
        for (k, vec) in cmsg.trailers.iter() {
            // Skip trailers whose keys are known sections.
            if !message_section_by_label(k).is_none()  {
                continue;
            }
            for v in vec.iter() {
                extra_trailers.push_str(&format!("{k}: {v}\n"));
            }
        }
    }
    if extra_trailers.len() > 0 {
        sections.insert(MessageSection::ExtraTrailers, extra_trailers);
    }

    Ok(sections)
}

/// Render a trailer section
///
/// If the trailer value has more than one line, the subsequent lines are
/// indented by a spaces, as described in https://git-scm.com/docs/git-interpret-trailers
fn render_trailer_section(section: &MessageSection, text: String) -> String {

    let mut ret = String::new();

    for (i, line) in text
        .split('\n')
        .enumerate()
    {
        if i == 0 {
            let label = message_section_label(section);
            ret.push_str(&format!("{}: {}\n", label, line));
        } else {
            ret.push_str(&format!(" {}\n", line));
        }
    }

    ret
}

pub fn build_message(
    section_texts: &MessageSectionsMap,
    desired_sections: &[MessageSection],
) -> String {
    let mut ret = String::new();
    let mut trailers = MessageSectionsMap::new();

    // Look only for the desired sections.
    for section in desired_sections {
        let value = section_texts.get(section);
        if value.is_none() {
            continue;
        }
        let text = value.unwrap();

        // If section is a trailer, just store it. We'll add
        // all trailers at the end of the message.
        if message_section_is_trailer(section) {
            let rendered_text = render_trailer_section(section, text.to_string());
            trailers.insert(*section, rendered_text);
            continue;
        }

        // Not a trailer, so it should be either the title or the summary.
        if section != &MessageSection::Title && section != &MessageSection::Summary {
            panic!("unexpected non-trailer section: {:?}", section);
        }

        // Section has no text, nothing to do here.
        if text.len() == 0 {
            continue;
        }

        // Add blank line separating previous "section" if needed.
        if ret.len() > 0 {
            ret.push_str("\n\n");
        }
        ret.push_str(text);
    }

    // Add extra blank line to separate the trailers paragraph.
    ret.push_str("\n\n");

    // Add known section trailers, if any.
    for (_, rendered_text) in &trailers {
        ret.push_str(&rendered_text);
    }

    // Add extra trailers, if any.
    if let Some(text) = section_texts.get(&MessageSection::ExtraTrailers) {
        ret.push_str(text);
    }

    // Make sure to keep just a single newline at the end of the message.
    ret = ret.trim_end().to_string();
    ret.push('\n');

    ret
}

pub fn build_commit_message(section_texts: &MessageSectionsMap) -> String {
    build_message(
        section_texts,
        &[
            MessageSection::Title,
            MessageSection::Summary,
            MessageSection::TestPlan,
            MessageSection::Reviewers,
            MessageSection::ReviewedBy,
            MessageSection::PullRequest,
        ],
    )
}

pub fn build_github_body(section_texts: &MessageSectionsMap) -> String {
    build_message(
        section_texts,
        &[MessageSection::Summary, MessageSection::TestPlan],
    )
}

pub fn build_github_body_for_merging(
    section_texts: &MessageSectionsMap,
) -> String {
    build_message(
        section_texts,
        &[
            MessageSection::Summary,
            MessageSection::TestPlan,
            MessageSection::Reviewers,
            MessageSection::ReviewedBy,
            MessageSection::PullRequest,
        ],
    )
}

pub fn validate_commit_message(
    message: &MessageSectionsMap,
    config: &crate::config::Config,
) -> Result<()> {
    if config.require_test_plan
        && !message.contains_key(&MessageSection::TestPlan)
    {
        output("💔", "Commit message does not have a Test Plan!")?;
        return Err(Error::empty());
    }

    let title_missing_or_empty = match message.get(&MessageSection::Title) {
        None => true,
        Some(title) => title.is_empty(),
    };
    if title_missing_or_empty {
        output("💔", "Commit message does not have a title!")?;
        return Err(Error::empty());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    fn must_parse(
        msg: &str,
        top_section: MessageSection,
    ) -> MessageSectionsMap {
        let sections = parse_message(msg, top_section);
        assert!(sections.is_ok(), "commit message parse error: msg={:?} error={:?}", msg, sections);
        sections.unwrap()
    }

    #[test]
    fn test_parse_empty() {
        assert_eq!(
            must_parse("", MessageSection::Title),
            [(MessageSection::Title, "".to_string())].into()
        );
    }

    #[test]
    fn test_parse_title() {
        assert_eq!(
            must_parse("Hello", MessageSection::Title),
            [(MessageSection::Title, "Hello".to_string())].into()
        );
        assert_eq!(
            must_parse("Hello\n", MessageSection::Title),
            [(MessageSection::Title, "Hello".to_string())].into()
        );
        assert_eq!(
            must_parse("\n\nHello\n\n", MessageSection::Title),
            [(MessageSection::Title, "Hello".to_string())].into()
        );
    }

    #[test]
    fn test_parse_title_and_summary() {
        assert_eq!(
            must_parse("Hello\nFoo Bar", MessageSection::Title),
            [
                (MessageSection::Title, "Hello".to_string()),
                (MessageSection::Summary, "Foo Bar".to_string())
            ]
            .into()
        );
        assert_eq!(
            must_parse("Hello\n\nFoo Bar", MessageSection::Title),
            [
                (MessageSection::Title, "Hello".to_string()),
                (MessageSection::Summary, "Foo Bar".to_string())
            ]
            .into()
        );
        assert_eq!(
            must_parse("Hello\n\n\nFoo Bar", MessageSection::Title),
            [
                (MessageSection::Title, "Hello".to_string()),
                (MessageSection::Summary, "Foo Bar".to_string())
            ]
            .into()
        );
        assert_eq!(
            must_parse("Hello\n\nFoo Bar", MessageSection::Title),
            [
                (MessageSection::Title, "Hello".to_string()),
                (MessageSection::Summary, "Foo Bar".to_string())
            ]
            .into()
        );
    }

    #[test]
    fn test_parse_sections() {
        assert_eq!(
            must_parse(
// Was:
//                 r#"Hello
//
// Test plan: testzzz
//
// Summary:
// here is
// the
// summary (it's not a "Test plan:"!)
//
// Reviewer:    a, b, c"#,
r#"Hello

here is
the
summary (it's not a "Test-Plan:"!)

Test-Plan: testzzz
Reviewers:    a, b, c
"#,
                MessageSection::Title
            ),
            [
                (MessageSection::Title, "Hello".to_string()),
                (
                    MessageSection::Summary,
                    // "here is\nthe\nsummary (it's not a \"Test plan:\"!)"
                    "here is\nthe\nsummary (it's not a \"Test-Plan:\"!)"
                        .to_string()
                ),
                (MessageSection::TestPlan, "testzzz".to_string()),
                (MessageSection::Reviewers, "a, b, c".to_string()),
            ]
            .into()
        );
    }

    // -----------------------------------------------------------------
    // build_message*() tests

    #[test]
    fn test_build_message_just_title() {
        assert_eq!(
            build_message(
                &must_parse(
                    r#"test: just title

"#,
                    MessageSection::Title,
                ),
                &[
                    MessageSection::Title,
                    MessageSection::Summary,
                ],
            ),

            "test: just title\n"
        );
    }

    // -------------------------------------------------
    #[test]
    fn test_build_message_just_title_and_summary() {
        assert_eq!(
            build_message(
                &must_parse(
                    r#"Just title and summary

Notice: not a trailer

More summary here

"#,
                    MessageSection::Title,
                ),
                &[
                    MessageSection::Title,
                    MessageSection::Summary,
                ],
            ),
            r#"Just title and summary

Notice: not a trailer

More summary here
"#,
        );
    }

    // -------------------------------------------------
    #[test]
    fn test_build_message_no_blank_between_title_and_summary() {
        assert_eq!(
            build_message(
                &must_parse(
                    r#"No blank line between title and summary
Summary"#,
                    MessageSection::Title,
                ),
                &[
                    MessageSection::Title,
                    MessageSection::Summary,
                ],
            ),
            r#"No blank line between title and summary

Summary
"#,
        );
    }

    // -------------------------------------------------
    #[test]
    fn test_build_message_just_title_and_known_trailer() {
        assert_eq!(
            build_message(
                &must_parse(
                    r#"Just title and known trailer

 Test-Plan: foobar
"#,
                    MessageSection::Title,
                ),
                &[
                    MessageSection::Title,
                    MessageSection::Summary,
                    MessageSection::TestPlan,
                ],
            ),
            r#"Just title and known trailer

Test-Plan: foobar
"#,
        );
    }

    // -------------------------------------------------
    #[test]
    fn test_build_message_title_summary_known_trailers() {
        assert_eq!(
            build_commit_message(
                &must_parse(
                    r#"test: title, summary and regular sections

Summary: not a trailer

http://example.com/foo2
  http://example.com/foo1

Reviewers: a, b, c
Test-Plan: Foo
 Bar
 Baz

"#,
                    MessageSection::Title,
                ),
            ),
            r#"test: title, summary and regular sections

Summary: not a trailer

http://example.com/foo2
  http://example.com/foo1

Test-Plan: Foo Bar Baz
Reviewers: a, b, c
"#,
        );
    }

    // -------------------------------------------------
    #[test]
    fn test_build_message_with_extra_trailers() {
        assert_eq!(
            build_commit_message(
                &must_parse(
                    r#"Title, summary, regular sections, extra sections

Summary

Notice: not a trailer

Extra1: extra1
Extra2: extra2
Reviewers: a, b, c
Test-Plan: Foo
 Bar
 Baz

"#,
                    MessageSection::Title,
                ),
            ),
            r#"Title, summary, regular sections, extra sections

Summary

Notice: not a trailer

Test-Plan: Foo Bar Baz
Reviewers: a, b, c
Extra1: extra1
Extra2: extra2
"#,
        );
    }

    // -------------------------------------------------
    // Build message requesting just summary: make sure
    // unknown trailers are still included.
    #[test]
    fn test_build_message_with_just_summary() {
        assert_eq!(
            build_message(
                &must_parse(
                    r#"Title will not show up in built message

Summary: not a trailer

 http://example.com/foo

Reviewers: a, b, c
Test-Plan: Foo
 Bar
 Baz
Extra-Trailer: extra trailer must not be discarded
"#,
                    MessageSection::Title,
                ),
                &[
                    MessageSection::Summary,  // <<< just summary requested
                ],
            ),
            r#"Summary: not a trailer

 http://example.com/foo

Extra-Trailer: extra trailer must not be discarded
"#,
        );
    }

}
