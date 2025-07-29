<system_context>
You are an expert AGENTS.md editor.
IMPORTANT: If you make any changes that makes any AGENTS.md file out of date,
please update the AGENTS.md file accordingly.
</system_context>

<critical_notes>
## MISSION CRITICAL RULES

1.  **Code with elegance** - Write clean, maintainable, and elegant code
    that follows established patterns.

2.  **Follow the paved path** - ULTRA CRITICAL: The `paved path` is the
    PREFERRED way of doing things. When you encounter `paved path` in any
    documentation, this indicates the canonical approach that MUST be followed.

3.  **User runs the application** - Unless you are running a QA command,
    you do not run the app. Always ask the user to run the app and report
    results back to you.

4.  **Clarify ambiguity** - Favor asking follow-up questions to ensure
    clear understanding of requirements before implementation.

5.  **Preserve existing functionality** - NEVER reduce the scope of existing
    features/behaviors unless explicitly instructed to do so.

6.  **AGENTS.md as living documentation** - ULTRA CRITICAL: Treat all
    AGENTS.md files as living API documentation for your future self.
    Always check for relevant AGENTS.md files and update them when changes
    impact their accuracy.

7.  **Writing expert AGENTS.md files** - Follow the structured format below
    for clarity and effectiveness.
</critical_notes>

<agents_md_best_practices>
## AGENTS.MD BEST PRACTICES

### Purpose & Philosophy

-   **Living brain**: AGENTS.md files are your persistent memory across sessions
-   **API documentation**: Write for your future self as an expert coding agent
-   **Token-aware**: Keep concise while preserving critical information
-   **Current state only**: Document what IS, not what WAS (no changelogs)

### Structure & Format

**Use aggressive newlines.** Lines should not be too long.
This improves readability and makes the document easier to read.

Use headings to break up sections.

#### XML-Style Tags (Semantic Sections)

```markdown
<system_context>
Brief overview of what this module/system does.
Set the stage for understanding.
</system_context>

<file_map>
## FILE MAP
- `/path/to/file` - Brief description
- `/path/to/folder/` - What's in this folder
</file_map>

<paved_path>
## ARCHITECTURE (PAVED PATH)
The canonical way to do things.
Battle-tested patterns that MUST be followed.
</paved_path>

<patterns>
## PATTERNS
Common patterns with real code examples from the codebase.
</patterns>

<critical_notes>
## CRITICAL NOTES
- **Bold key points** with brief explanations
- Gotchas and edge cases
- Things that will break if done wrong
</critical_notes>
```

#### 1. Code Examples

```rust
// Bad: excessive unwrapping
fn get_name(user_id: u32) -> String {
    let user = get_user(user_id).unwrap();
    user.name.unwrap()
}

// Good: Chain methods instead
fn get_name(user_id: u32) -> String {
    get_user(user_id)
        .and_then(|user| user.name)
        .unwrap_or_else(|| "Unknown".to_string())
}
```

#### 2. Writing Style

-   **Terse but complete**: Every word matters
-   **Present tense**: "Store manages state" not "Store will manage"
-   **Active voice**: "Use this pattern" not "This pattern should be used"
-   **Imperatives for rules**: "MUST", "NEVER", "ALWAYS"

### Advanced Techniques

#### Chain of Thought

```markdown
<workflow>
## WORKFLOW
1. **Find interface** in `/shared/interfaces/market-data-provider.ts`
2. **Create directory** `/integrations/providers/YourProvider/`
3. **Create files** implement provider interface and rate limiting
   per `/integrations/providers/AGENTS.md`
</workflow>
```
</agents_md_best_practices>
