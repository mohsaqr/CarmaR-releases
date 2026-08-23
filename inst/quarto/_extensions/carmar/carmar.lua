local function scalar(value, fallback)
  if value == nil then return fallback end
  local text = pandoc.utils.stringify(value)
  if text == "" then return fallback end
  return text
end

local function html_escape(value)
  return value:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;")
    :gsub('"', "&quot;"):gsub("'", "&#39;")
end

function Meta(meta)
  if not quarto.doc.is_format("html:js") then return meta end

  local options = type(meta.carmar) == "table" and meta.carmar or {}
  local port = tonumber(scalar(options.port, "4747")) or 4747
  if port < 1 or port > 65535 or port ~= math.floor(port) then port = 4747 end
  local label = scalar(options.label, "Run on my computer")

  quarto.doc.add_html_dependency({
    name = "carmar-published",
    version = "0.1.0",
    scripts = {{ path = "carmar-publish.js", defer = true }},
    stylesheets = {"carmar-publish.css"}
  })
  quarto.doc.include_text("in-header",
    '<meta name="carmar-port" content="' .. tostring(port) .. '">\n' ..
    '<meta name="carmar-label" content="' .. html_escape(label) .. '">')
  return meta
end
