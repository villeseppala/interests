// The saved settings of xriskb.html: its Author controls and what readers first see. Written by the
// page's Save settings (source("app_xriskb/author.R"), or open it as xriskb.html?author); hand edits are
// fine. The build (tools/build_static.R) puts this into the published page. Anything left out keeps
// the page's default.
window.XRISKB_SETTINGS = {
  "author": {
    "--g-bg": "#1e2933",
    "--g-line-col": "#000000",
    "--row-line-w": "2px",
    "--row-line-col": "#030303",
    "--v-bg": "#152b3c",
    "--v-edit-bg": "#02111c",
    "--title-fs": "19px",
    "--formula-fs": "17.2px",
    "--ar-w": "1.6px",
    "potInPop": true,
    "graphYears": true
  },
  "view": {
    "points": [
      2,
      3
    ],
    "zoom": 1.022,
    "boxText": 1.3,
    "namesWidth": 385,
    "variables": [
      "rate",
      "surv",
      "popexp",
      "lives"
    ],
    "show": {
      "graphs": true,
      "arrows": true,
      "spin": false,
      "formulas": true,
      "change": true,
      "b": true,
      "names": false,
      "comparison": false
    },
    "comparePrevious": false,
    "excludePopulation": true,
    "graphScale": {
      "rate": "cum",
      "surv": "full",
      "years": "potall",
      "popexp": "pot",
      "lives": "potall",
      "pop": "all"
    },
    "views": {
      "rateComp": true,
      "survComp": true,
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
