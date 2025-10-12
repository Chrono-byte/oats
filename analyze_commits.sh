#!/bin/bash

# Script to analyze commits and suggest better commit messages
# This will help identify what each commit actually changed

echo "=== OATS PROJECT COMMIT ANALYSIS ==="
echo "Analyzing the last 20 commits for content and suggesting improved messages..."
echo ""

# Function to analyze a commit and suggest a better message
analyze_commit() {
    local commit_hash=$1
    local current_msg="$(git log --format=%s -n 1 $commit_hash)"
    
    echo "----------------------------------------"
    echo "Commit: $commit_hash"
    echo "Current: $current_msg"
    echo "Date: $(git log --format=%ad -n 1 $commit_hash)"
    echo ""
    
    # Get file changes
    echo "Files changed:"
    git show --name-only $commit_hash | grep -v "^commit\|^Author\|^Date\|^$\|^    "
    echo ""
    
    # Get stats
    echo "Stats:"
    git show --stat $commit_hash | tail -n 1
    echo ""
    
    # Analyze the type of changes
    local files_changed=$(git show --name-only $commit_hash | grep -v "^commit\|^Author\|^Date\|^$\|^    " | wc -l)
    local rust_files=$(git show --name-only $commit_hash | grep "\.rs$" | wc -l)
    local test_files=$(git show --name-only $commit_hash | grep "tests/" | wc -l)
    local example_files=$(git show --name-only $commit_hash | grep "examples/" | wc -l)
    local doc_files=$(git show --name-only $commit_hash | grep -E "\.(md|txt)$" | wc -l)
    
    # Analyze specific file patterns and changes
    local codegen_files=$(git show --name-only $commit_hash | grep "codegen/" | wc -l)
    local runtime_files=$(git show --name-only $commit_hash | grep "runtime/" | wc -l)
    local parser_files=$(git show --name-only $commit_hash | grep "parser" | wc -l)
    
    echo "Analysis:"
    echo "- Total files: $files_changed"
    echo "- Rust files: $rust_files"
    echo "- Test files: $test_files"
    echo "- Example files: $example_files"
    echo "- Doc files: $doc_files"
    echo "- Codegen files: $codegen_files"
    echo "- Runtime files: $runtime_files"
    echo "- Parser files: $parser_files"
    
    # Try to determine the nature of changes
    local additions=$(git show --stat $commit_hash | tail -n 1 | grep -o '[0-9]* insertion' | cut -d' ' -f1)
    local deletions=$(git show --stat $commit_hash | tail -n 1 | grep -o '[0-9]* deletion' | cut -d' ' -f1)
    
    echo "- Lines added: ${additions:-0}"
    echo "- Lines deleted: ${deletions:-0}"
    echo ""
}

# Get the last 20 commits
commits=$(git log --oneline -20 | cut -d' ' -f1)

for commit in $commits; do
    analyze_commit $commit
done

echo "=== ANALYSIS COMPLETE ==="
echo ""
echo "To rewrite commit messages, you can use:"
echo "git rebase -i HEAD~20  # for last 20 commits"
echo "git rebase -i <commit-hash>  # for specific range"