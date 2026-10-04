# Linear Walkthrough

Generate a detailed, file-by-file linear walkthrough of this codebase that explains how everything works.

## Instructions

### Step 1: Read the entire codebase

- Use `find` or `git ls-files` to discover all source files (excluding vendored deps, node_modules, lock files, build artifacts, .git, etc.)
- Read every source file to build a complete mental model of the codebase
- Note file sizes, dependencies between files, and the overall architecture

### Step 2: Plan the walkthrough order

Before writing anything, plan the logical order to present files. Good ordering strategies:
- **Data model first**: Start with core types/models, then services, then UI/API layers
- **Entry point first**: Start from the main entry point and trace outward
- **Dependency order**: Present files so that by the time a file is discussed, everything it depends on has already been explained

Write your plan out and confirm the ordering makes sense before proceeding.

### Step 3: Create the walkthrough document

Create a file called `WALKTHROUGH.md` in the repo root. Structure it as follows:

#### Header
- Project name and one-line description
- Total codebase size (number of files, total lines of code)
- Key technologies/frameworks used

#### Project Structure
- Show the file tree (use `find` or `tree` command output)
- Show line counts per file (use `wc -l`)

#### Walkthrough of how it all works
- 


#### How It All Connects
A final summary section that traces key flows through the codebase:
- The main data flow (e.g., request → handler → service → database → response)
- How components communicate
- The lifecycle of key operations

## Important Rules

- **NEVER manually copy code into the document.** Always use `uvx showboat`. Run `uvx showboat --help`
- Keep commentary informative but concise — explain the "why" not just the "what"
- Use the project's own terminology and naming conventions
- If the codebase is very large (>50 files), focus on the most important files and note which files were skipped and why
- The walkthrough should be a learning document — someone reading it should understand not just what the code does, but how to work in this codebase

$ARGUMENTS
