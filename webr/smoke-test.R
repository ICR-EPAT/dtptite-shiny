# Smoke test of the Shinylive build: serve `_site/`, open it in headless
# Chrome and wait until the app has run on its example data, that is the
# summary boxes, both plots and the model table have rendered and no
# output shows an error.
#
# Run from the repository root after webr/export.R: Rscript webr/smoke-test.R
timeout <- as.numeric(Sys.getenv("SMOKE_TIMEOUT", "300"))

port <- httpuv::randomPort()
server <- httpuv::startServer(
  "127.0.0.1", port,
  list(staticPaths = list("/" = httpuv::staticPath("_site")))
)
on.exit(httpuv::stopServer(server), add = TRUE)

session <- chromote::ChromoteSession$new()
on.exit(session$close(), add = TRUE)
invisible(session$Page$navigate(sprintf("http://127.0.0.1:%d/", port)))

# The app runs in an iframe; search every same-origin frame for it.
status_js <- "
(() => {
  const docs = [];
  const walk = d => {
    docs.push(d);
    for (const f of d.querySelectorAll('iframe')) {
      try { if (f.contentDocument) walk(f.contentDocument); } catch (e) {}
    }
  };
  walk(document);
  const app = docs.find(d => d.getElementById('model_table'));
  if (!app) return 'loading';
  const errors = [...app.querySelectorAll('.shiny-output-error')]
    .filter(e => !e.classList.contains('shiny-output-error-validation'));
  if (errors.length) {
    return 'error: ' + errors.map(e => e.id + ': ' + e.textContent.trim()).join('; ');
  }
  if (app.getElementById('summary_boxes').textContent.trim() === '') {
    return 'loading';
  }
  // Shiny only renders outputs on the visible tab, so open the tab of the
  // first output that has not rendered yet.
  const outputs = [
    ['timeline_plot', 'svg'], ['dose_toxicity_plot', 'svg'],
    ['model_table', 'table']
  ];
  for (const [id, tag] of outputs) {
    if (app.querySelector('#' + id + ' ' + tag)) continue;
    const pane = app.getElementById(id).closest('.tab-pane');
    if (pane && !pane.classList.contains('active')) {
      app.querySelector('[data-bs-toggle=\"tab\"][href=\"#' + pane.id + '\"]').click();
    }
    return 'loading';
  }
  return 'ready';
})()
"

start <- Sys.time()
repeat {
  status <- session$Runtime$evaluate(status_js)$result$value
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  if (!identical(status, "loading")) break
  if (elapsed > timeout) {
    stop("App did not render within ", timeout, " seconds.", call. = FALSE)
  }
  Sys.sleep(2)
}
if (!identical(status, "ready")) stop("App rendered with ", status, call. = FALSE)
message(sprintf("App rendered in %.0f seconds.", elapsed))
