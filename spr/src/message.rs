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
) -> MessageSectionsMap {

    let msg = orig_msg.trim();

    let mut sections = MessageSectionsMap::new();

    // Parse the commit message and populate the sections map based on
    // what was required. First, the title and summary.
    let cmsg = parse_commit_message(msg);

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

    sections
}

pub fn build_message(
    section_texts: &MessageSectionsMap,
    sections: &[MessageSection],
) -> String {
    let mut result = String::new();
    let mut display_label = false;

    for section in sections {
        let value = section_texts.get(section);
        if let Some(text) = value {
            if !result.is_empty() {
                result.push('\n');
            }

            if section != &MessageSection::Title
                && section != &MessageSection::Summary
            {
                // Once we encounter a section that's neither Title nor Summary,
                // we start displaying the labels.
                display_label = true;
            }

            if display_label {
                let label = message_section_label(section);
                result.push_str(label);
                result.push_str(
                    if label.len() + text.len() > 76 || text.contains('\n') {
                        ":\n"
                    } else {
                        ": "
                    },
                );
            }

            result.push_str(text);
            result.push('\n');
        }
    }

    result
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

    #[test]
    fn test_parse_empty() {
        assert_eq!(
            parse_message("", MessageSection::Title),
            [(MessageSection::Title, "".to_string())].into()
        );
    }

    #[test]
    fn test_parse_title() {
        assert_eq!(
            parse_message("Hello", MessageSection::Title),
            [(MessageSection::Title, "Hello".to_string())].into()
        );
        assert_eq!(
            parse_message("Hello\n", MessageSection::Title),
            [(MessageSection::Title, "Hello".to_string())].into()
        );
        assert_eq!(
            parse_message("\n\nHello\n\n", MessageSection::Title),
            [(MessageSection::Title, "Hello".to_string())].into()
        );
    }

    #[test]
    fn test_parse_title_and_summary() {
        assert_eq!(
            parse_message("Hello\nFoo Bar", MessageSection::Title),
            [
                (MessageSection::Title, "Hello".to_string()),
                (MessageSection::Summary, "Foo Bar".to_string())
            ]
            .into()
        );
        assert_eq!(
            parse_message("Hello\n\nFoo Bar", MessageSection::Title),
            [
                (MessageSection::Title, "Hello".to_string()),
                (MessageSection::Summary, "Foo Bar".to_string())
            ]
            .into()
        );
        assert_eq!(
            parse_message("Hello\n\n\nFoo Bar", MessageSection::Title),
            [
                (MessageSection::Title, "Hello".to_string()),
                (MessageSection::Summary, "Foo Bar".to_string())
            ]
            .into()
        );
        assert_eq!(
            parse_message("Hello\n\nFoo Bar", MessageSection::Title),
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
            parse_message(
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

Here is
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
                    "Here is\nthe\nsummary (it's not a \"Test-Plan:\"!)"
                        .to_string()
                ),
                (MessageSection::TestPlan, "testzzz".to_string()),
                (MessageSection::Reviewers, "a, b, c".to_string()),
            ]
            .into()
        );
    }
}
