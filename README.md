# carmar

Notebooks and runnable documents on your own local R.

```r
install.packages("carmar")
```

Three verbs:

```r
carmar::run()                              # open the notebook on your R
carmar::listen("https://book.example")     # let a published page run chunks here
carmar::use_publishing("path/to/project")  # make a Quarto project's chunks runnable
```

`run()` starts a local kernel - a separate R process - and opens a
self-contained notebook page in the browser. `listen()` keeps a kernel
waiting on a fixed port for the R chunks of a published page.
`use_publishing()` installs the Quarto filter that gives those chunks a Run
button, and `check_publishing()` verifies the rendered site. `sessions()`
and `stop_kernel()` list and stop what is running.

Nothing in the package contacts the network. On your explicit call it writes
its state under `tools::R_user_dir("carmar")` and one discovery record under
`~/.carmar/run`, removed on a clean stop.

The desktop application (a menu-bar helper, file associations, a
double-click launcher) is a separate product and is not part of this
package; it comes from <https://lacarm.com/carmar/>.

See `vignette("publishing", package = "carmar")` for the author's and the
reader's stories.

## Publishing without R at hand

The Quarto extension is also a plain folder: download `carmar-quarto.zip` from
<https://lacarm.com/carmar/>, unzip it inside the book (it creates
`_extensions/carmar/`), and add `- carmar` under `filters:` in `_quarto.yml`.
`use_publishing()` does exactly that from R.

## Pages that are already rendered

`make_runnable("report.html")` or `make_runnable("_book")` inserts one
`<script>` tag before `</head>` of each HTML file, once. No re-render: the
runtime gives every R block a Run button and a strip at the top of the page.
