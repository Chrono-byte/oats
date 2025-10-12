#!/bin/bash

# Script to help rewrite commit messages
# This creates the rebase todo file with the improved messages

echo "=== OATS PROJECT COMMIT MESSAGE REWRITER ==="
echo ""

# Check if we're in a git repository
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: Not in a git repository"
    exit 1
fi

# Create backup branch
echo "Creating backup branch..."
git branch backup-before-rewrite 2>/dev/null
if [ $? -eq 0 ]; then
    echo "✓ Backup branch 'backup-before-rewrite' created"
else
    echo "ℹ Backup branch already exists"
fi

echo ""
echo "IMPORTANT: This will rewrite commit history!"
echo "Commits to be rewritten (last 20):"
echo "========================================"
git log --oneline -20
echo ""

read -p "Do you want to proceed with rewriting commit messages? (y/N): " confirm

if [[ $confirm != [yY] && $confirm != [yY][eE][sS] ]]; then
    echo "Aborted."
    exit 0
fi

# Create a mapping file for the improved messages
cat > /tmp/oats_commit_messages.txt << 'EOF'
62609bb:docs: standardize documentation comments across codebase with unified voice
1a82c33:feat: implement escape analysis optimization and enhance codegen modules
0e1f153:feat: major codegen refactoring with generics support improvements
deaa9a9:feat: comprehensive compiler overhaul with const evaluation and cycle collection
b3a0571:refactor: major cleanup and documentation improvements across project
96506df:test: disable failing tests pending implementation fixes
db83a8c:refactor: cleanup examples and consolidate test infrastructure
c2ff480:test: fix failing codegen tests and improve test reliability
d927317:feat: enhance TypeScript compatibility in parser and codegen
6411838:docs: comprehensive documentation overhaul and fuzzing infrastructure
ce2d40b:chore: update runtime dependencies and configurations
a7b1f2a:fix: resolve clippy warning in codegen emit module
08b2483:fix: resolve second clippy warning in codegen emit module
1d5c393:fix: resolve clippy warnings across codebase
e327a13:merge: integrate async-await branch with conflict resolution in codegen
d97ea97:feat: implement async-await support in codegen pipeline
674a27a:perf: optimize performance issues in codegen and runtime
edfb02e:style: apply code formatting across codegen and runtime modules
955db56:refactor: complete runtime library rewrite in Rust
88ec012:refactor: migrate improve runtime safety
EOF

echo ""
echo "Starting interactive rebase..."
echo "In the editor that opens:"
echo "1. Change 'pick' to 'reword' for commits you want to update"
echo "2. Save and close the editor"
echo "3. For each commit, you'll be prompted to enter the new message"
echo ""
echo "Suggested new messages are available in /tmp/oats_commit_messages.txt"
echo ""

# Start the interactive rebase
git rebase -i HEAD~20

echo ""
echo "Rebase complete!"
echo "Check the result with: git log --oneline -20"
echo ""
echo "If something went wrong, you can restore with:"
echo "git reset --hard backup-before-rewrite"