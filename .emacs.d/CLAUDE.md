# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Structure

This is a personal Emacs configuration using a centralized approach with two main files:

- `init.el` - Main Emacs configuration file containing all package configurations and customizations
- `early-init.el` - Early initialization file handling native compilation setup and performance optimizations
- `straight/` - Package manager directory (managed by straight.el, don't edit directly)
- Additional directories:
  - `savefile/` - Directory for saving Emacs state files
  - `eln-cache/` - Native compilation cache
  - `tree-sitter/` - Tree-sitter grammar files

## Package Management

This configuration uses **straight.el** as the package manager with `use-package` for configuration:

- Packages are installed and managed through straight.el
- Configuration uses `use-package` declarations throughout `init.el`
- Bootstrap code for straight.el is included in `init.el`

## Key Configuration Patterns

### Custom Functions Prefix
All custom functions are prefixed with `rchrand/` (e.g., `rchrand/goto-init-file`, `rchrand/project-buffers-list`)

### Major Package Categories
1. **Project Management**: Projectile with Counsel integration
2. **Completion Framework**: Ivy/Counsel/Swiper ecosystem
3. **UI Enhancements**: Doom themes, rainbow-delimiters, which-key
4. **Editor Improvements**: Anzu, expand-region, avy, undo-tree
5. **Version Control**: Magit, git-timemachine
6. **Org Mode**: Comprehensive org-mode setup with agenda and capture templates
7. **Language Support**: Markdown, YAML, Zig modes

### Key Custom Keybindings
- `C-c I` - Open init.el file
- `C-c T` - Open todo.org file
- `C-c R` - Reload init.el
- `C-c F` - Find file in project
- `C-c S` - Smart project search
- `C-c p` - Projectile command map
- `M-p` - Perspective mode prefix

## Development Workflow

### Configuration Management
- Edit `init.el` directly for configuration changes
- Use `C-c R` to reload configuration without restarting Emacs
- For early initialization changes, edit `early-init.el`

### Testing Configuration Changes
- Use `emacs -Q` to start Emacs without configuration for debugging
- Use `emacs --debug-init` to debug initialization issues
- Check `*Messages*` buffer for errors and warnings

### Project Management
- Uses Projectile for project detection and management
- Project switching via `C-c p p`
- Project search with counsel-projectile-rg via `C-c p /`
- Current project setup references `~/landfolk/` directory

### Org Mode Workflow
- Agenda files located in `~/org/` directory
- Todo capture templates configured for `~/org/todo.org` and `~/org/inbox.org`
- Custom TODO states: STARTED, WAITING, TODO → DONE, CANCELED, DELEGATED, SOMEDAY

## macOS-Specific Configuration

The configuration is optimized for macOS:
- Homebrew paths for native compilation (libgccjit)
- macOS modifier key mappings (Command → Meta, Option → Super)
- Font fallback chain for macOS fonts (JetBrains Mono preferred)
- exec-path-from-shell for proper environment variable loading

## Common Commands

### Emacs Commands
```bash
emacs                    # Start Emacs with full configuration
emacs -Q                 # Start without configuration (debugging)
emacs --debug-init       # Start with debugging enabled
emacs -nw               # Start in terminal mode
```

### Configuration Editing
- Open configuration: `C-c I` (from within Emacs)
- Reload configuration: `C-c R` (from within Emacs)
- Navigate to specific sections by searching for `use-package` declarations

## Performance Optimizations

The configuration includes several performance optimizations:
- Garbage collection tuning in `early-init.el`
- Process output buffer size increase
- Projectile caching enabled
- Native compilation setup for macOS

## Important File Locations

- Main config: `/Users/runehessner/.emacs.d/init.el`
- Early init: `/Users/runehessner/.emacs.d/early-init.el`
- Org files: `~/org/` (todo.org, inbox.org, agenda files)
- Project data: `/Users/runehessner/.emacs.d/projects.eld`
- Recent files: `/Users/runehessner/.emacs.d/recentf.eld`
