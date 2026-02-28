# Local Development

### Set Up Dependencies

First run
```bash
nix develop
```
to load all the dependencies into your shell.

Then run
```bash
./scripts/build_and_install.sh
```
You only need to do this once, it will set up PGRX's dev environment

### Test Run Extension

To test the extension you can run
```bash
cargo pgrx run # This won't work
```
_But_ this won't work, because the extension relies on the semver extension and
I haven't figured out how to install it into PGRX's dev environment. So to work
around it I've implemented some of the semver functionality myself. Go into
`lib.rs` and uncomment the `mod semver;` line and then go to
`pg_statecharts.control` and replace `requires = 'ltree, semver'` with
`requires = 'ltree'`.

Now `cargo pgrx run` will work.
