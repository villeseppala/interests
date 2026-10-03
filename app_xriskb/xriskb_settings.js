// The saved settings of xriskb.html: its Author controls and what readers first see. Written by the
// page's Save settings (source("app_xriskb/author.R"), or open it as xriskb.html?author); hand edits are
// fine. The build (tools/build_static.R) puts this into the published page. Anything left out keeps
// the page's default.
window.XRISKB_SETTINGS = {
  "author": {
    "--g-bg": "#1e2933",
    "--g-line-col": "#000000",
    "--row-line-w": "1px",
    "--row-line-col": "#030303",
    "potInPop": true
  },
  "view": {
    "horizon": 2,
    "first": 1,
    "zoom": 0.769,
    "boxText": 1.4,
    "namesWidth": 298,
    "variables": [
      "rate",
      "surv",
      "popexp",
      "lives"
    ],
    "show": {
      "graphs": true,
      "arrows": true,
      "spin": true,
      "formulas": true,
      "change": true,
      "b": true,
      "names": false,
      "comparison": true
    },
    "comparePrevious": false,
    "excludePopulation": true,
    "graphScale": {
      "surv": "all",
      "years": "potall",
      "popexp": "pot",
      "lives": "potall",
      "pop": "all"
    },
    "views": {
      "rateComp": true,
      "survComp": false,
      "yearsComp": true,
      "yearsGens": false,
      "livesLy": false,
      "livesLoss": false
    },
    "relative": {
      "rate": true
    }
  }
};
