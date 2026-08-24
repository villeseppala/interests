# ============================================================================
# Shiny — hazard/survival threshold controls (value-space, bidirectional).
# ----------------------------------------------------------------------------
# Curve interpolated in VALUE space (cubic Hermite on value vs fx), mapped
# through the y-axis for drawing, so it bends where it crosses a decade band.
#
# Two control sets, one authoritative at a time ("driver"):
#   * Hazard set (red): anchors value+slope, per-segment controls -> hazard(fx),
#     survival derived by S=prod(1-h)^dy.
#   * Survival set (blue): same handles on the survival curve -> survival(fx),
#     hazard derived by h = 1 - (S_i/S_{i-1})^(1/dy).
# Grabbing a handle makes its set the driver; the other set is greyed and just
# tracks the derived curve.  Default hazard peaks around year 20.
#
# Run:  shiny::runApp("hazard_thresholds.R")
# ============================================================================

required <- c("shiny", "ggplot2")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

library(shiny)
library(ggplot2)

## ---- constants + shared helpers -------------------------------------------
SPANX <- c(10, 90, 900, 9000); M <- 100; Wpx <- 1240; Hpx <- 698   # 4 periods to 12026, width matches x-risk app
YEARS   <- c(2026, 2036, 2126, 3026, 12026)                        # checkpoint calendar years (same as x-risk)
year_lbl <- function(y) if (y >= 10000) format(y, big.mark = " ", scientific = FALSE) else as.character(y)

haz_to_fy <- function(h) {
  h <- pmin(1e-1, pmax(1e-5, h)); d <- floor(log10(h)); seg <- pmin(3, pmax(0, d + 5))
  lo <- 10^(seg-5); hi <- 10^(seg-4); (seg + pmin(1, pmax(0, (h-lo)/(hi-lo)))) / 4
}
fit_linexp <- function(mx) max(-5, min(-1, floor(log10(mx)) + 1))
herm <- function(px, x0, y0, m0, x1, y1, m1) {
  h <- x1 - x0; u <- (px-x0)/h
  y0*(2*u^3-3*u^2+1) + m0*h*(u^3-2*u^2+u) + y1*(-2*u^3+3*u^2) + m1*h*(u^3-u^2)
}
default_atheta <- function(anchors) {
  fy <- haz_to_fy(anchors); px <- (0:4)/4*Wpx; py <- (1-fy)*Hpx; th <- numeric(5)
  for (i in 1:5) { lo <- max(1,i-1); hi <- min(5,i+1); th[i] <- atan2(py[hi]-py[lo], px[hi]-px[lo]) }
  th
}
sample_set <- function(anchors, thetaL, thetaR, fxc, cval, useCtrl, dYdv, clampV, log_time = FALSE) {
  tfrac <- function(u) if (log_time) (10^u - 1) / 9 else u   # log: single decade of time per span
  rows <- list()
  for (s in 1:4) {
    fxL <- (s-1)/4; fxR <- s/4; hL <- anchors[s]; hR <- anchors[s+1]
    mL <- tan(thetaR[s])*Wpx/dYdv(hL); mR <- tan(thetaL[s+1])*Wpx/dYdv(hR); mC <- (hR-hL)/(fxR-fxL)
    fxC <- fxc[s]; hC <- cval[s]; ms <- if (s==1) 0:M else 1:M; fx <- fxL + (ms/M)*(fxR-fxL)
    u <- ms/M; dy <- SPANX[s] * (tfrac(u) - tfrac(u - 1/M))   # years covered by each sample
    v <- if (useCtrl) ifelse(fx<=fxC, herm(fx,fxL,hL,mL,fxC,hC,mC), herm(fx,fxC,hC,mC,fxR,hR,mR))
         else herm(fx,fxL,hL,mL,fxR,hR,mR)
    rows[[s]] <- data.frame(fx = fx, val = clampV(v), dy = dy)
  }
  do.call(rbind, rows)
}
haz_dYdv <- function(h, lm, linearY) {
  if (linearY) return(-Hpx/lm)
  h <- pmin(1e-1, pmax(1e-5, h)); seg <- pmin(3, pmax(0, floor(log10(h))+5))
  lo <- 10^(seg-5); hi <- 10^(seg-4); -(Hpx/4)/(hi-lo)
}
compute_both <- function(hz, sv, driver, useCtrl, linearY, log_time = FALSE) {
  if (driver == "hazard") {
    lm <- if (linearY) 10^fit_linexp(max(hz$anchors)) else 1
    hs <- sample_set(hz$anchors, hz$thetaL, hz$thetaR, hz$fxc, hz$cval, useCtrl,
                     function(h) haz_dYdv(h, lm, linearY), function(v) pmin(1e-1, pmax(1e-5, v)), log_time)
    surv <- cumprod((1 - hs$val)^hs$dy)
  } else {
    ss <- sample_set(sv$anchors, sv$thetaL, sv$thetaR, sv$fxc, sv$cval, useCtrl,
                     function(v) -Hpx, function(v) pmin(1, pmax(0, v)), log_time)
    ss$val <- cummin(pmax(1e-6, ss$val))                  # survival is non-increasing
    prev <- c(1, head(ss$val, -1)); r <- pmin(1, pmax(1e-9, ss$val/prev))
    hs <- data.frame(fx = ss$fx, val = pmin(1e-1, pmax(1e-5, 1 - r^(1/ss$dy))), dy = ss$dy)
    surv <- ss$val
    lm <- if (linearY) 10^fit_linexp(max(hs$val)) else 1
  }
  fy_of <- function(v) if (linearY) pmin(1, pmax(0, v/lm)) else haz_to_fy(v)
  data.frame(fx = hs$fx, fy = fy_of(hs$val), hz = hs$val, surv = surv)
}

## ---- JS overlay -----------------------------------------------------------
overlay_js <- HTML(r"---(
(function(){
  const NS="http://www.w3.org/2000/svg";
  const HAZ="#e8836f", SUR="#4fb8d6", HBAR="#c96f5c", SBAR="#3f9ab5";
  const POP="#e46ba6", PBAR="#a84e7d";
  const CYR="#6fcf97", CLY="#e8b04f", CLV="#b98fe0";   // cumulative: survived years / life-years / lives
  let popMax=1e10;    // population axis ceiling; grows by decades (10 B, 100 B, ...)
  const POP_DEFAULT={ anchors:[8.3e9,8.3e9,8.3e9,8.3e9,8.3e9], thetaL:[0,0,0,0,0], thetaR:[0,0,0,0,0], fxc:[1/8,3/8,5/8,7/8], cval:[8.3e9,8.3e9,8.3e9,8.3e9] };
  const PAL=["#1a9850","#91cf60","#fdae61","#d73027"];
  const YRS=[2026,2036,2126,3026,12026];
  const SPANX=[10,90,900,9000], M=100, GRAB=15, LEV=34, LIN_EMIN=-5, LIN_EMAX=-1;
  let showPoints=true, useCtrl=false, split=true, linearY=true, logTime=false, driver="hazard";
  let visHaz=true, visSurv=true, visPop=true, visCumYr=false, visCumLy=false, visCumLives=false;
  let minorGrid=[];
  function tFrac(u){ return logTime ? (Math.pow(10,u)-1)/9 : u; }   // log: single decade of time per span
  let linExp=-1, linMax=0.1, grewReach=false;   // grewReach: population one-grow-per-reach guard
  let W,H, hazSet={anchors:[],ctrls:[]}, survSet={anchors:[],ctrls:[]}, popSet={anchors:[],ctrls:[]}, hazS=[], survS=[], popS=[];
  let yTicks=[], popTicks=[], cumYrTicks=[], cumLyTicks=[], cumLvTicks=[], infoTx=[], bandsG, bandSig="", svg, label, hazLine, surLine, popLine, expLine, readySent=false, drag=null;
  let baseHazLine, baseSurLine, basePopLine, baseExpLine, cumYrLine, cumLyLine, cumLvLine;
  let baseCumYrLine, baseCumLyLine, baseCumLvLine;
  let baseMode="previous", baseSnap=null, prevSnap=null;   // comparison: "none" | "current" | "previous"

  const clampHaz=h=>Math.max(1e-5,Math.min(1e-1,h)), clamp01=v=>Math.max(0,Math.min(1,v)), sX=fx=>fx*W;
  function hazToFyDec(h){ h=clampHaz(h); let seg=Math.min(3,Math.max(0,Math.floor(Math.log10(h))+5));
    const lo=Math.pow(10,seg-5),hi=Math.pow(10,seg-4); return (seg+Math.max(0,Math.min(1,(h-lo)/(hi-lo))))/4; }
  const sYh=h=>linearY?(1-clamp01(h/linMax))*H:(1-hazToFyDec(h))*H;
  const hazFromFy=fy=>linearY?clampHaz(clamp01(fy)*linMax):(function(){let seg=Math.min(3,Math.floor(clamp01(fy)*4)),frac=clamp01(fy)*4-seg,
    lo=Math.pow(10,seg-5),hi=Math.pow(10,seg-4);return lo+frac*(hi-lo);})();
  const sYs=s=>(1-s)*H;
  const ratio=h=>Math.round(1/clampHaz(h)).toLocaleString();
  function foldClamp(a){ if(a>Math.PI/2)a-=Math.PI; else if(a<-Math.PI/2)a+=Math.PI; return Math.max(-1.35,Math.min(1.35,a)); }
  function herm(px,x0,y0,m0,x1,y1,m1){ const h=x1-x0,u=(px-x0)/h;
    return y0*(2*u*u*u-3*u*u+1)+m0*h*(u*u*u-2*u*u+u)+y1*(-2*u*u*u+3*u*u)+m1*h*(u*u*u-u*u); }
  function dScreenYdh(h){ if(linearY) return -H/linMax; h=clampHaz(h);
    const seg=Math.min(3,Math.max(0,Math.floor(Math.log10(h))+5)),lo=Math.pow(10,seg-5),hi=Math.pow(10,seg-4); return -(H/4)/(hi-lo); }
  // variable descriptors
  const HDESC={ yOf:sYh,        invY:py=>hazFromFy(1-py/H), clampV:clampHaz, dYdv:dScreenYdh, col:HAZ, bar:HBAR, ratioLbl:v=>"1 in "+ratio(v)+" ("+(v*100).toFixed(4)+"%)" };
  const SDESC={ yOf:s=>(1-s)*H, invY:py=>clamp01(1-py/H),   clampV:clamp01,  dYdv:()=> -H,      col:SUR, bar:SBAR, ratioLbl:v=>(v*100).toFixed(1)+"% surv" };
  const sYp = p=>(1-clamp01(p/popMax))*H;
  const PDESC={ yOf:sYp, invY:py=>clamp01(1-py/H)*popMax, clampV:p=>Math.max(0,Math.min(1e15,p)), dYdv:()=>-H/popMax, col:POP, bar:PBAR, ratioLbl:v=>Math.round(v/1e6).toLocaleString()+" M pop" };
  function maxPopVal(){ let m=0; popSet.anchors.forEach(a=>{ if(a.val>m)m=a.val; }); popS.forEach(p=>{ if(p.val>m)m=p.val; }); return m; }
  function growPop(){ if(maxPopVal()<0.8*popMax) grewReach=false;                        // value came back down → re-arm
    if(!grewReach && maxPopVal()>=popMax && popMax<1e14){ popMax*=10; grewReach=true; } }
  function fitPop(){ const mx=Math.max(maxPopVal(), baseMaxPop()); popMax=Math.max(1e10, Math.pow(10, Math.ceil(Math.log10(Math.max(1,mx))))); }
  // comparison baseline snapshot (sampled value arrays, aligned by index)
  function snap(){ return { fx:hazS.map(p=>p.fx), haz:hazS.map(p=>p.val), surv:survS.map(p=>p.val), pop:popS.map(p=>p.val), dy:popS.map(p=>p.dy) }; }
  function activeBase(){ return baseMode==="previous"?prevSnap:(baseMode==="current"?baseSnap:null); }
  function curveValAt2(fxA,valA,fx){ if(fx<=fxA[0])return valA[0]; for(let i=1;i<fxA.length;i++){ if(fxA[i]>=fx){ const t=(fx-fxA[i-1])/(fxA[i]-fxA[i-1]); return valA[i-1]+t*(valA[i]-valA[i-1]); } } return valA[valA.length-1]; }
  // stack labels so higher value sits higher (smaller y) with a minimum gap
  function stackByValue(items,gap){ items.sort((a,b)=>b.val-a.val); for(let i=1;i<items.length;i++){ if(items[i].y<items[i-1].y+gap) items[i].y=items[i-1].y+gap; } return items; }
  function updateCmpBtns(){ const c=document.getElementById("baseCur"), p=document.getElementById("basePrev");
    if(c) c.classList.toggle("active", baseMode==="current"); if(p) p.classList.toggle("active", baseMode==="previous"); }
  function setByName(n){ return n==="hazard"?hazSet:(n==="pop"?popSet:survSet); }
  function descByName(n){ return n==="hazard"?HDESC:(n==="pop"?PDESC:SDESC); }
  function fmtM(v){ return Math.round(v/1e6).toLocaleString(); }
  function fmtBig(v){ const a=Math.abs(v); if(a>=1e12)return (v/1e12).toFixed(2)+" T"; if(a>=1e9)return (v/1e9).toFixed(2)+" B"; if(a>=1e6)return (v/1e6).toFixed(1)+" M"; return Math.round(v).toLocaleString(); }

  function mk(t,a){ const e=document.createElementNS(NS,t); for(const k in a) e.setAttribute(k,a[k]); return e; }
  function mouse(e){ const r=svg.getBoundingClientRect(); return {x:(e.clientX-r.left)*(W/r.width), y:(e.clientY-r.top)*(H/r.height)}; }
  function setFont(px){ let st=document.getElementById("ovFont"); if(!st){ st=document.createElement("style"); st.id="ovFont"; document.head.appendChild(st); }
    st.textContent="#overlay text{font-size:"+px+"px;}"; }   // one CSS rule overrides every SVG label
  function maxHaz(){ let m=0; hazSet.anchors.forEach(a=>{ if(a.val>m) m=a.val; }); return m; }
  // when a baseline is shown, the scale must also cover its values so the ghost lines aren't clipped
  function baseMaxHaz(){ const B=activeBase(); let m=0; if(B) B.haz.forEach(v=>{ if(v>m) m=v; }); return m; }
  function baseMaxPop(){ const B=activeBase(); let m=0; if(B) B.pop.forEach(v=>{ if(v>m) m=v; }); return m; }
  function setLinExp(e){ e=Math.max(LIN_EMIN,Math.min(LIN_EMAX,e)); linExp=e; linMax=Math.pow(10,e); }
  function fitLinExp(){ setLinExp(Math.floor(Math.log10(Math.max(maxHaz(), baseMaxHaz())))+1); }

  function paletteColor(i){ return PAL[Math.max(0,Math.min(3,i))]; }
  function rebuildBands(){ bandsG.innerHTML="";
    if(!linearY){ for(let b=0;b<4;b++){ const yBot=(1-b/4)*H,yTop=(1-(b+1)/4)*H,hgt=yBot-yTop;
      bandsG.appendChild(mk("rect",{x:0,y:yTop,width:W,height:hgt,fill:"none",stroke:paletteColor(b),"stroke-width":1.5,opacity:0.85}));
      if(b>0) for(let k=1;k<=10;k++){ const yy=yBot-(k/10)*hgt;
        bandsG.appendChild(mk("line",{x1:0,y1:yy,x2:W,y2:yy,stroke:paletteColor(b-1),"stroke-width":1,opacity:0.28})); } } }
    else { bandsG.appendChild(mk("rect",{x:0,y:0,width:W,height:H,fill:"none",stroke:paletteColor(linExp+4),"stroke-width":1.5,opacity:0.85}));
      for(let k=1;k<=10;k++){ const yy=(1-k/10)*H; bandsG.appendChild(mk("line",{x1:0,y1:yy,x2:W,y2:yy,stroke:paletteColor(linExp+3),"stroke-width":1,opacity:0.28})); } } }
  function updateYTicks(){ for(let k=0;k<yTicks.length;k++){ const fy=k/4;
    let txt; if(linearY){ const h=fy*linMax; txt=h<=0?"0":"1 in "+Math.round(1/h).toLocaleString(); yTicks[k].setAttribute("fill",HAZ); }
    else { txt="1 in "+Math.pow(10,5-k).toLocaleString(); yTicks[k].setAttribute("fill",paletteColor(Math.min(3,k))); } yTicks[k].textContent=txt; } }

  function sampleSet(set, desc){
    const out=[];
    for(let s=0;s<4;s++){ const fxL=s/4,fxR=(s+1)/4,hL=set.anchors[s].val,hR=set.anchors[s+1].val;
      const mL=Math.tan(set.anchors[s].thetaR)*W/desc.dYdv(hL), mR=Math.tan(set.anchors[s+1].thetaL)*W/desc.dYdv(hR);
      const fxC=set.ctrls[s].fxc,hC=set.ctrls[s].val,mC=(hR-hL)/(fxR-fxL);
      for(let m=(s===0?0:1);m<=M;m++){ const fx=fxL+(m/M)*(fxR-fxL), dy=SPANX[s]*(tFrac(m/M)-tFrac((m-1)/M));
        let v=useCtrl?(fx<=fxC?herm(fx,fxL,hL,mL,fxC,hC,mC):herm(fx,fxC,hC,mC,fxR,hR,mR)):herm(fx,fxL,hL,mL,fxR,hR,mR);
        out.push({fx:fx,val:desc.clampV(v),dy:dy}); } }
    return out;
  }
  function deriveSurv(hs){ let s=1; return hs.map(p=>{ s*=Math.pow(1-p.val,p.dy); return {fx:p.fx,val:clamp01(s),dy:p.dy}; }); }
  function deriveHaz(ss){ let prev=1; return ss.map(p=>{ const r=Math.max(1e-9,Math.min(1,p.val/prev)); const h=clampHaz(1-Math.pow(r,1/p.dy)); prev=p.val; return {fx:p.fx,val:h,dy:p.dy}; }); }
  function curveValAt(sm,fx){ if(fx<=sm[0].fx) return sm[0].val;
    for(let i=1;i<sm.length;i++){ if(sm[i].fx>=fx){ const t=(fx-sm[i-1].fx)/(sm[i].fx-sm[i-1].fx); return sm[i-1].val+t*(sm[i].val-sm[i-1].val); } } return sm[sm.length-1].val; }
  function syncSet(set, sm, desc){
    set.anchors.forEach(a=>{ a.val=curveValAt(sm,a.fx);
      const v0=curveValAt(sm,Math.max(0,a.fx-0.01)), v1=curveValAt(sm,Math.min(1,a.fx+0.01));
      const th=foldClamp(Math.atan2(desc.yOf(v1)-desc.yOf(v0), 0.02*W)); a.thetaL=th; a.thetaR=th; });
    set.ctrls.forEach(k=>{ k.val=curveValAt(sm,k.fxc); }); }

  function recompute(){
    popS=sampleSet(popSet,PDESC);                    // population is an independent input curve
    if(driver==="hazard"){ if(linearY) fitLinExp();
      hazS=sampleSet(hazSet,HDESC); survS=deriveSurv(hazS); syncSet(survSet,survS,SDESC); }
    else { survS=sampleSet(survSet,SDESC); let prev=1; survS.forEach(p=>{ p.val=Math.min(prev,Math.max(1e-6,p.val)); prev=p.val; });
      hazS=deriveHaz(survS); if(linearY){ let mx=baseMaxHaz(); hazS.forEach(p=>{ if(p.val>mx) mx=p.val; }); setLinExp(Math.floor(Math.log10(mx))+1); }
      syncSet(hazSet,hazS,HDESC); }
    while(baseMaxPop()>popMax&&popMax<1e14) popMax*=10;   // grow pop scale to cover the baseline
  }

  function styleSet(set, desc, active, hidden){
    let base=active?1:0.4, op=showPoints?base:0, oc=useCtrl?(showPoints?(active?1:base*0.6):0):0;
    if(hidden){ op=0; oc=0; }
    const dc=active?desc.col:"#bbb", bc=active?desc.bar:"#ccc";
    set.anchors.forEach(a=>{ const cx=sX(a.fx), cy=desc.yOf(a.val);
      const eRx=cx+LEV*Math.cos(a.thetaR),eRy=cy+LEV*Math.sin(a.thetaR),eLx=cx-LEV*Math.cos(a.thetaL),eLy=cy-LEV*Math.sin(a.thetaL);
      a.barL.setAttribute("x1",cx);a.barL.setAttribute("y1",cy);a.barL.setAttribute("x2",eLx);a.barL.setAttribute("y2",eLy);a.barL.setAttribute("stroke",bc);a.barL.setAttribute("opacity",op);
      a.barR.setAttribute("x1",cx);a.barR.setAttribute("y1",cy);a.barR.setAttribute("x2",eRx);a.barR.setAttribute("y2",eRy);a.barR.setAttribute("stroke",bc);a.barR.setAttribute("opacity",op);
      a.dot.setAttribute("cx",cx);a.dot.setAttribute("cy",cy);a.dot.setAttribute("fill",dc);a.dot.setAttribute("opacity",op); });
    set.ctrls.forEach(k=>{ k.body.setAttribute("cx",sX(k.fxc));k.body.setAttribute("cy",desc.yOf(k.val));
      k.body.setAttribute("stroke",active?"#666":"#bbb");k.body.setAttribute("opacity",oc); }); }

  function redraw(){
    const sig=linearY?("L"+linExp):"T"; if(sig!==bandSig){ rebuildBands(); bandSig=sig; }
    recompute(); updateYTicks();
    hazLine.setAttribute("points", hazS.map(p=>sX(p.fx)+","+sYh(p.val)).join(" ")); hazLine.setAttribute("opacity", visHaz?1:0);
    surLine.setAttribute("points", survS.map(p=>sX(p.fx)+","+sYs(p.val)).join(" ")); surLine.setAttribute("opacity", visSurv?1:0);
    popLine.setAttribute("points", popS.map(p=>sX(p.fx)+","+sYp(p.val)).join(" ")); popLine.setAttribute("opacity", visPop?1:0);
    expLine.setAttribute("points", popS.map((p,k)=>sX(p.fx)+","+sYp(p.val*survS[k].val)).join(" ")); expLine.setAttribute("opacity", visPop?1:0);   // expected = potential x survival
    popTicks.forEach(t=>t.el.textContent=fmtBig(t.fr*popMax));
    const gu = logTime ? [2,3,4,5,6,7,8,9].map(v=>Math.log10(v)) : [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9];
    minorGrid.forEach((ln,idx)=>{ const s=Math.floor(idx/9), j=idx%9;
      if(j<gu.length){ const gx=sX((s+gu[j])/4); ln.setAttribute("x1",gx); ln.setAttribute("x2",gx); ln.setAttribute("opacity",1); } else ln.setAttribute("opacity",0); });
    styleSet(hazSet,HDESC,true); styleSet(survSet,SDESC,false,true); styleSet(popSet,PDESC,true);   // survival hidden, pop active
    // comparison baseline curves (semi-transparent)
    const B=activeBase();
    if(B){ baseHazLine.setAttribute("points", B.haz.map((v,k)=>sX(B.fx[k])+","+sYh(v)).join(" ")); baseHazLine.setAttribute("opacity", visHaz?0.4:0);
      baseSurLine.setAttribute("points", B.surv.map((v,k)=>sX(B.fx[k])+","+sYs(v)).join(" ")); baseSurLine.setAttribute("opacity", visSurv?0.4:0);
      basePopLine.setAttribute("points", B.pop.map((v,k)=>sX(B.fx[k])+","+sYp(v)).join(" ")); basePopLine.setAttribute("opacity", visPop?0.4:0);
      baseExpLine.setAttribute("points", B.pop.map((v,k)=>sX(B.fx[k])+","+sYp(v*B.surv[k])).join(" ")); baseExpLine.setAttribute("opacity", visPop?0.4:0); }
    else { [baseHazLine,baseSurLine,basePopLine,baseExpLine].forEach(l=>l.setAttribute("points","")); }
    for(let i=0;i<5;i++){ const fx=i/4, px=sX(fx), anc=i===0?"start":(i===4?"end":"middle"), it=infoTx[i];
      const h=curveValAt(hazS,fx), s=curveValAt(survS,fx);
      const hb=B?curveValAt2(B.fx,B.haz,fx):null, sb=B?curveValAt2(B.fx,B.surv,fx):null;
      // hazard readout blocks (new + baseline), higher value higher
      let hz=[{val:h, y:Math.max(2,sYh(h)-32), base:false}];
      if(B) hz.push({val:hb, y:Math.max(2,sYh(hb)-32), base:true});
      stackByValue(hz,34);
      hz.forEach(o=>{ const a1=o.base?it.bh1:it.h1, a2=o.base?it.bh2:it.h2, hop=visHaz?(o.base?0.5:1):0;
        a1.setAttribute("x",px);a1.setAttribute("y",o.y);a1.setAttribute("text-anchor",anc);a1.setAttribute("opacity",hop);a1.textContent="1 in "+ratio(o.val);
        a2.setAttribute("x",px);a2.setAttribute("y",o.y+16);a2.setAttribute("text-anchor",anc);a2.setAttribute("opacity",hop);a2.textContent=(o.val*100).toFixed(4)+"%"; });
      if(!B){ it.bh1.textContent=""; it.bh2.textContent=""; }
      // survival labels (new + baseline), higher value higher
      let sv=[{val:s, y:Math.min(H-6,sYs(s)+22), base:false}];
      if(B) sv.push({val:sb, y:Math.min(H-6,sYs(sb)+22), base:true});
      stackByValue(sv,16);
      sv.forEach(o=>{ const el=o.base?it.bsurv:it.surv; el.setAttribute("x",px);el.setAttribute("y",o.y);el.setAttribute("text-anchor",anc);
        el.setAttribute("opacity",(!visSurv||o.val>=0.99995)?0:(o.base?0.5:1)); el.textContent=(o.val*100).toFixed(2)+"%"; });
      if(!B) it.bsurv.textContent=""; }
    // population outputs below the plot (with baseline "was ..." when comparing)
    let cumLY=[], acc=0;
    for(let k=0;k<popS.length;k++){ acc += popS[k].val*survS[k].val*popS[k].dy; cumLY.push({fx:popS[k].fx,val:acc}); }
    let bCum=null; if(B){ bCum=[]; let ba=0; for(let k=0;k<B.pop.length;k++){ ba+=B.pop[k]*B.surv[k]*B.dy[k]; bCum.push({fx:B.fx[k],val:ba}); } }
    // cumulative curves: survived years, life-years, lives — each normalized to its own final value (shape only)
    let cumYr=[], accYr=0;
    for(let k=0;k<popS.length;k++){ accYr += survS[k].val*popS[k].dy; cumYr.push({fx:popS[k].fx,val:accYr}); }
    // scale max includes the baseline so previous ghost lines are always fully shown
    let bYr=null, maxYr=cumYr[cumYr.length-1].val||1, maxLy=cumLY[cumLY.length-1].val||1;
    if(B){ bYr=[]; let bya=0; for(let k=0;k<B.pop.length;k++){ bya+=B.surv[k]*B.dy[k]; bYr.push({fx:B.fx[k],val:bya}); }
      maxYr=Math.max(maxYr, bYr[bYr.length-1].val||1); maxLy=Math.max(maxLy, bCum[bCum.length-1].val||1); }
    const bnorm=(arr,mx)=>arr.map(p=>sX(p.fx)+","+((1-p.val/mx)*H)).join(" ");
    cumYrLine.setAttribute("points", visCumYr?bnorm(cumYr,maxYr):"");
    cumLyLine.setAttribute("points", visCumLy?bnorm(cumLY,maxLy):"");
    cumLvLine.setAttribute("points", visCumLives?bnorm(cumLY,maxLy):"");   // lives ∝ life-years → same normalized shape
    if(B){ baseCumYrLine.setAttribute("points", visCumYr?bnorm(bYr,maxYr):"");
      baseCumLyLine.setAttribute("points", visCumLy?bnorm(bCum,maxLy):"");
      baseCumLvLine.setAttribute("points", visCumLives?bnorm(bCum,maxLy):""); }
    else { [baseCumYrLine,baseCumLyLine,baseCumLvLine].forEach(l=>l.setAttribute("points","")); }
    // right-side scale labels (top=max, mid, bottom=0) — reflect the shared scale max
    function setLane(ticks, mx, on, unit){ const vals=[mx, mx/2, 0];
      ticks.forEach((t,k)=>{ t.setAttribute("opacity",on?1:0); t.textContent=on?(fmtBig(vals[k])+unit):""; }); }
    setLane(cumYrTicks, maxYr,    visCumYr,     " yr");
    setLane(cumLyTicks, maxLy,    visCumLy,     " ly");
    setLane(cumLvTicks, maxLy/75, visCumLives,  " lv");
    const po=document.getElementById("popout");
    if(po){ let rows="";
      for(let i=0;i<5;i++){ const fx=i/4, pot=curveValAt(popS,fx), s2=curveValAt(survS,fx), ex=pot*s2, ly=curveValAt(cumLY,fx);
        const pos = i===0 ? "left:0;text-align:left;" : (i===4 ? "right:0;text-align:right;" : "left:"+(fx*100)+"%;transform:translateX(-50%);text-align:center;");
        let c="<div class='pcell' style='position:absolute;top:0;"+pos+"'><b>"+YRS[i].toLocaleString()+"</b><br>potential "+fmtM(pot)+" M<br>expected "+fmtM(ex)+" M<br>"+fmtBig(ly)+" life-yrs<br>"+fmtBig(ly/75)+" lives";
        if(B){ const bp=curveValAt2(B.fx,B.pop,fx), bs=curveValAt2(B.fx,B.surv,fx), bly=curveValAt(bCum,fx);
          c+="<br><span class='was'>was "+fmtM(bp)+" / exp "+fmtM(bp*bs)+" M &middot; "+fmtBig(bly/75)+" lives</span>"; }
        c+="</div>"; rows+=c; }
      po.innerHTML=rows; } }

  function makeSet(){ const set={anchors:[],ctrls:[]};
    for(let s=0;s<4;s++){ const k={fxc:0,val:0}; k.body=mk("circle",{r:8,fill:"rgba(255,255,255,0.92)",stroke:"#666","stroke-width":1.5,style:"cursor:move;"}); svg.appendChild(k.body); set.ctrls.push(k); }
    for(let i=0;i<5;i++){ const a={fx:i/4,val:0,thetaL:0,thetaR:0};
      a.barL=mk("line",{"stroke-width":8,"stroke-linecap":"round",style:"cursor:grab;"});
      a.barR=mk("line",{"stroke-width":8,"stroke-linecap":"round",style:"cursor:grab;"});
      a.dot =mk("circle",{r:6,stroke:"#fff","stroke-width":1.5,style:"cursor:ns-resize;"});
      svg.appendChild(a.barL); svg.appendChild(a.barR); svg.appendChild(a.dot); set.anchors.push(a); } return set; }
  function loadSet(set,d){ set.anchors.forEach((a,i)=>{ a.val=d.anchors[i]; a.thetaL=d.thetaL[i]; a.thetaR=d.thetaR[i]; });
    set.ctrls.forEach((k,s)=>{ k.fxc=d.fxc[s]; k.val=d.cval[s]; }); }

  function build(d){
    W=d.W; H=d.H;
    svg=document.getElementById("overlay"); svg.setAttribute("viewBox","0 0 "+W+" "+H); svg.innerHTML="";
    bandsG=mk("g",{}); svg.appendChild(bandsG); bandSig="";
    minorGrid=[];                                    // minor time gridlines (positioned per log/linear in redraw)
    for(let s=0;s<4;s++) for(let k=0;k<9;k++){ const ln=mk("line",{y1:0,y2:H,stroke:"rgba(255,255,255,0.08)"}); svg.appendChild(ln); minorGrid.push(ln); }
    for(let s=1;s<4;s++) svg.appendChild(mk("line",{x1:sX(s/4),y1:0,x2:sX(s/4),y2:H,stroke:"rgba(255,255,255,0.18)"}));
    yTicks=[];
    for(let k=0;k<=4;k++){ const py=(1-k/4)*H;
      const th=mk("text",{x:6,y:py-3,"font-size":11,fill:HAZ}); svg.appendChild(th); yTicks.push(th);
      const ts=mk("text",{x:W-6,y:py-3,"text-anchor":"end","font-size":11,fill:SUR}); ts.textContent=(k/4).toFixed(2); svg.appendChild(ts); }
    popTicks=[];
    [[1,10],[0.5,H/2],[0,H-4]].forEach(fy=>{ const t=mk("text",{x:W-72,y:fy[1],"text-anchor":"end","font-size":10,fill:POP}); svg.appendChild(t); popTicks.push({el:t,fr:fy[0]}); });
    // right-side lanes for the cumulative variables (max / mid / 0), further in so they don't overlap survival & population
    function mkLane(x,col){ const arr=[]; [12,H/2,H-4].forEach(y=>{ const t=mk("text",{x:x,y:y,"text-anchor":"end","font-size":10,fill:col,opacity:0}); svg.appendChild(t); arr.push(t); }); return arr; }
    cumYrTicks=mkLane(W-150,CYR); cumLyTicks=mkLane(W-250,CLY); cumLvTicks=mkLane(W-250,CLV);
    baseHazLine=mk("polyline",{fill:"none",stroke:HAZ,"stroke-width":1.6,opacity:0.4,points:""}); svg.appendChild(baseHazLine);
    baseSurLine=mk("polyline",{fill:"none",stroke:SUR,"stroke-width":1.6,opacity:0.4,points:""}); svg.appendChild(baseSurLine);
    basePopLine=mk("polyline",{fill:"none",stroke:POP,"stroke-width":1.6,opacity:0.4,points:""}); svg.appendChild(basePopLine);
    baseExpLine=mk("polyline",{fill:"none",stroke:POP,"stroke-width":1.6,opacity:0.4,points:"","stroke-dasharray":"6,4"}); svg.appendChild(baseExpLine);
    hazLine=mk("polyline",{fill:"none",stroke:HAZ,"stroke-width":1.8,points:""}); svg.appendChild(hazLine);
    surLine=mk("polyline",{fill:"none",stroke:SUR,"stroke-width":1.8,points:""}); svg.appendChild(surLine);
    popLine=mk("polyline",{fill:"none",stroke:POP,"stroke-width":1.8,points:""}); svg.appendChild(popLine);
    expLine=mk("polyline",{fill:"none",stroke:POP,"stroke-width":1.8,points:"","stroke-dasharray":"6,4"}); svg.appendChild(expLine);
    baseCumYrLine=mk("polyline",{fill:"none",stroke:CYR,"stroke-width":1.6,opacity:0.4,points:""}); svg.appendChild(baseCumYrLine);
    baseCumLyLine=mk("polyline",{fill:"none",stroke:CLY,"stroke-width":1.6,opacity:0.4,points:""}); svg.appendChild(baseCumLyLine);
    baseCumLvLine=mk("polyline",{fill:"none",stroke:CLV,"stroke-width":1.6,opacity:0.4,points:""}); svg.appendChild(baseCumLvLine);
    cumYrLine=mk("polyline",{fill:"none",stroke:CYR,"stroke-width":2,points:""}); svg.appendChild(cumYrLine);   // cumulative survived years
    cumLyLine=mk("polyline",{fill:"none",stroke:CLY,"stroke-width":2,points:""}); svg.appendChild(cumLyLine);   // cumulative life-years
    cumLvLine=mk("polyline",{fill:"none",stroke:CLV,"stroke-width":2,points:""}); svg.appendChild(cumLvLine);   // cumulative lives
    survSet=makeSet(); hazSet=makeSet(); popSet=makeSet();
    loadSet(hazSet,d.haz); loadSet(survSet,d.surv); loadSet(popSet,POP_DEFAULT);
    infoTx=[];                                          // persistent readouts at each threshold
    for(let i=0;i<5;i++){
      const bh1=mk("text",{"text-anchor":"middle",fill:HAZ,opacity:0.5}), bh2=mk("text",{"text-anchor":"middle",fill:HAZ,opacity:0.5}), bts=mk("text",{"text-anchor":"middle",fill:SUR,opacity:0.5});
      const h1=mk("text",{"text-anchor":"middle","font-weight":"bold",fill:HAZ});   // row 1: 1 in x
      const h2=mk("text",{"text-anchor":"middle","font-weight":"bold",fill:HAZ});   // row 2: probability %
      const ts=mk("text",{"text-anchor":"middle","font-weight":"bold",fill:SUR});   // survival %
      svg.appendChild(bh1); svg.appendChild(bh2); svg.appendChild(bts);
      svg.appendChild(h1); svg.appendChild(h2); svg.appendChild(ts);
      infoTx.push({h1:h1,h2:h2,surv:ts,bh1:bh1,bh2:bh2,bsurv:bts}); }
    fitLinExp(); redraw();
    prevSnap=snap();                                 // seed a "previous" baseline so it shows before any drag
    const bc=document.getElementById("baseCur"), bp=document.getElementById("basePrev");
    if(bc) bc.onclick=()=>{ baseMode = baseMode==="current"?"none":"current"; if(baseMode==="current") baseSnap=snap(); updateCmpBtns(); redraw(); };
    if(bp) bp.onclick=()=>{ baseMode = baseMode==="previous"?"none":"previous"; if(baseMode==="previous"&&!prevSnap) prevSnap=snap(); updateCmpBtns(); redraw(); };
    updateCmpBtns();
    label=mk("text",{"font-size":12,"font-weight":"bold","text-anchor":"middle"}); svg.appendChild(label);
    attach();
  }

  function pick(mx,my){ let best=null,bd=GRAB*GRAB;
    const consider=(set,desc,name)=>{
      set.anchors.forEach((a,i)=>{ const cx=sX(a.fx),cy=desc.yOf(a.val);
        const eRx=cx+LEV*Math.cos(a.thetaR),eRy=cy+LEV*Math.sin(a.thetaR),eLx=cx-LEV*Math.cos(a.thetaL),eLy=cy-LEV*Math.sin(a.thetaL);
        let dx=mx-eRx,dy=my-eRy,d=dx*dx+dy*dy; if(d<bd){bd=d;best={set:name,type:"rR",i:i};}
        dx=mx-eLx;dy=my-eLy;d=dx*dx+dy*dy; if(d<bd){bd=d;best={set:name,type:"rL",i:i};}
        dx=mx-cx;dy=my-cy;d=dx*dx+dy*dy; if(d<bd){bd=d;best={set:name,type:"a",i:i};} });
      if(useCtrl) set.ctrls.forEach((k,s)=>{ const cx=sX(k.fxc),cy=desc.yOf(k.val),dx=mx-cx,dy=my-cy,d=dx*dx+dy*dy; if(d<bd){bd=d;best={set:name,type:"m",i:s};} }); };
    consider(hazSet,HDESC,"hazard"); consider(popSet,PDESC,"pop"); return best; }   // survival output-only

  function commit(){ Shiny.setInputValue("curve_set", {driver:driver,
    haz:{anchors:hazSet.anchors.map(a=>a.val),thetaL:hazSet.anchors.map(a=>a.thetaL),thetaR:hazSet.anchors.map(a=>a.thetaR),fxc:hazSet.ctrls.map(k=>k.fxc),cval:hazSet.ctrls.map(k=>k.val)},
    surv:{anchors:survSet.anchors.map(a=>a.val),thetaL:survSet.anchors.map(a=>a.thetaL),thetaR:survSet.anchors.map(a=>a.thetaR),fxc:survSet.ctrls.map(k=>k.fxc),cval:survSet.ctrls.map(k=>k.val)},
    t:Date.now()}, {priority:"event"}); }

  // scale grow/shrink with detach: dragging a value past the ceiling grows the scale a
  // decade and snaps the value to 105% of the old ceiling; dragging the whole curve down
  // into the bottom decade shrinks the scale and snaps to 95% of the new ceiling. Returns
  // {val,msg} when it fired (caller applies val + detaches), else null.
  function scaleSnap(setName, curVal, otherMax){
    if(setName==="hazard"&&linearY){
      if(curVal>=linMax*0.99999&&linExp<LIN_EMAX){ const o=linMax; setLinExp(linExp+1);
        return {val:clampHaz(1.05*o), msg:"scale → 1 in "+ratio(linMax)+"; value = 105% of old ceiling — re-grab"}; }
      if(curVal<=linMax*0.1&&otherMax<linMax*0.1&&linExp>LIN_EMIN){ setLinExp(linExp-1);
        return {val:clampHaz(0.95*linMax), msg:"scale → 1 in "+ratio(linMax)+"; value = 95% of new ceiling — re-grab"}; }
    } else if(setName==="pop"){
      if(curVal>=popMax*0.99999&&popMax<1e14){ const o=popMax; popMax*=10;
        return {val:Math.max(0,1.05*o), msg:"pop scale → "+fmtBig(popMax)+"; value = 105% of old ceiling — re-grab"}; }
      if(curVal<=popMax*0.1&&otherMax<popMax*0.1&&popMax>1e10){ popMax/=10;
        return {val:Math.max(0,0.95*popMax), msg:"pop scale → "+fmtBig(popMax)+"; value = 95% of new ceiling — re-grab"}; }
    }
    return null;
  }

  function attach(){
    svg.onpointerdown = e => { const m=mouse(e), p=pick(m.x,m.y); if(!p) return;
      e.preventDefault(); prevSnap=snap(); drag=p; svg.setPointerCapture(e.pointerId); redraw(); };   // snapshot pre-drag; driver stays "hazard"
    svg.onpointermove = e => {
      if(!drag) return; const m=mouse(e), set=setByName(drag.set), desc=descByName(drag.set), i=drag.i;
      const cx=sX(set.anchors[i]?set.anchors[i].fx:0), cy=set.anchors[i]?desc.yOf(set.anchors[i].val):0;
      if(drag.type==="a"){
        const nv=desc.clampV(desc.invY(m.y));
        let mo=0; set.anchors.forEach((a,k)=>{ if(k!==i && a.val>mo) mo=a.val; });   // other anchors (for shrink check)
        mo=Math.max(mo, drag.set==="hazard"?baseMaxHaz():(drag.set==="pop"?baseMaxPop():0));   // don't shrink below the baseline
        const sn=scaleSnap(drag.set, nv, mo);
        if(sn){ set.anchors[i].val=sn.val; commit(); drag=null; redraw(); document.getElementById("live").textContent=sn.msg; return; }
        set.anchors[i].val=nv; redraw();
        document.getElementById("live").textContent=drag.set+" "+YRS[i]+": "+desc.ratioLbl(set.anchors[i].val);
      } else if(drag.type==="rR"){ const a=foldClamp(Math.atan2(m.y-cy,m.x-cx)); set.anchors[i].thetaR=a; if(!split) set.anchors[i].thetaL=a; redraw();
        document.getElementById("live").textContent=drag.set+" "+YRS[i]+(split?": outgoing slope":": slope");
      } else if(drag.type==="rL"){ const a=foldClamp(Math.atan2(cy-m.y,cx-m.x)); set.anchors[i].thetaL=a; if(!split) set.anchors[i].thetaR=a; redraw();
        document.getElementById("live").textContent=drag.set+" "+YRS[i]+(split?": incoming slope":": slope");
      } else { const s=i, pxL=sX(s/4), pxR=sX((s+1)/4);
        const nv=desc.clampV(desc.invY(m.y));
        let mo=0; set.anchors.forEach(a=>{ if(a.val>mo) mo=a.val; });
        mo=Math.max(mo, drag.set==="hazard"?baseMaxHaz():(drag.set==="pop"?baseMaxPop():0));
        const sn=scaleSnap(drag.set, nv, mo);
        if(sn){ set.ctrls[s].val=sn.val; commit(); drag=null; redraw(); document.getElementById("live").textContent=sn.msg; return; }
        set.ctrls[s].fxc=Math.max((pxL+10)/W,Math.min((pxR-10)/W,m.x/W)); set.ctrls[s].val=nv; redraw();
        document.getElementById("live").textContent=drag.set+" segment "+(s+1)+" control: through "+desc.ratioLbl(set.ctrls[s].val); }
    };
    const end = e => { if(!drag) return; if(linearY) fitLinExp(); fitPop(); label.textContent=""; commit();
      document.getElementById("live").textContent="Released ("+driver+"-driven) — ggplot re-rendered."; drag=null; redraw(); };
    svg.onpointerup=end; svg.onpointercancel=end;
  }

  function sendReady(){ if(readySent) return; readySent=true; Shiny.setInputValue("client_ready", Date.now()); }
  document.addEventListener("shiny:connected", sendReady);
  setTimeout(sendReady, 500);
  Shiny.addCustomMessageHandler("init_points", build);
  Shiny.addCustomMessageHandler("set_curve", function(d){ driver=d.driver; loadSet(hazSet,d.haz); loadSet(survSet,d.surv); loadSet(popSet,POP_DEFAULT); popMax=1e10; if(linearY) fitLinExp(); redraw(); });
  Shiny.addCustomMessageHandler("set_params", function(p){ showPoints=p.points; useCtrl=p.useCtrl; linearY=p.linearY; logTime=p.logTime; const w=split; split=p.split;
    visHaz=p.visHaz; visSurv=p.visSurv; visPop=p.visPop; visCumYr=p.visCumYr; visCumLy=p.visCumLy; visCumLives=p.visCumLives;
    if(p.font) setFont(p.font);
    if(!split&&w){ [hazSet,survSet].forEach(S=>S.anchors.forEach(a=>{ const avg=(a.thetaL+a.thetaR)/2; a.thetaL=avg; a.thetaR=avg; })); }
    if(svg){ if(linearY) fitLinExp(); redraw(); commit(); } });
})();
)---")

## ---- UI -------------------------------------------------------------------
# a row of the 5 checkpoint years, positioned at the graph's x fractions (0,1/4,…,1)
year_row <- function() do.call(tags$div, c(list(class = "yearrow"),
  lapply(0:4, function(i) {
    pos <- if (i == 0) "left:0;text-align:left;"
           else if (i == 4) "right:0;text-align:right;"
           else sprintf("left:%s%%;transform:translateX(-50%%);text-align:center;", format(i / 4 * 100))
    tags$span(class = "ylbl", style = paste0("position:absolute;", pos), year_lbl(YEARS[i + 1]))
  })))

ui <- fluidPage(
  tags$head(tags$style(HTML(
    "body,.container-fluid{background:#0b3552; color:#e8eef4; font-family:Arial,Helvetica,sans-serif;}
     h3{color:#e8eef4; font-weight:700;}
     a{color:#78e6e7;}
     label,.control-label,.checkbox label{color:#e8eef4; font-weight:400;}
     .btn,.btn-default{background:#12405f; color:#e8eef4; border:1px solid #2a6a8f;}
     .btn:hover,.btn-default:hover{background:#175981; color:#fff;}
     .irs--shiny .irs-line{background:#12405f; border-color:#12405f;}
     .irs--shiny .irs-bar{background:#4fb8d6; border-color:#4fb8d6;}
     .irs--shiny .irs-min,.irs--shiny .irs-max,.irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single{color:#0b3552; background:#4fb8d6;}
     .irs--shiny .irs-grid-text{color:#9fb0bf;}
     .irs--shiny .irs-handle>i:first-child{background:#e8eef4;}
     #plot img{display:block; width:100%; height:100%;} #plot{line-height:0; position:absolute; inset:0; width:100%; height:100%;}
     #overlay{position:absolute; top:0; left:0; width:100%; height:100%; z-index:10; touch-action:none;}
     #overlay text{user-select:none;}
     #popout .pcell{line-height:1.55; max-width:210px;}
     #popout .pcell b{color:#f18fbe;}
     #popout .was{color:#9a6580; font-style:italic;}
     .cmprow{margin:4px 0 8px;}
     .cmpbtn{background:#12405f;color:#e8eef4;border:1px solid #2a6a8f;border-radius:6px;padding:6px 12px;font-size:13px;cursor:pointer;margin-right:8px;}
     .cmpbtn:hover{background:#175981;}
     .cmpbtn.active{background:#1f77a8;border-color:#8fd6f2;color:#fff;box-shadow:0 0 0 1px #8fd6f2 inset;}
     .legendrow{display:flex;align-items:center;gap:2px 22px;flex-wrap:wrap;margin:4px 0 10px;font-size:13.5px;}
     .legendrow .checkbox,.legendrow>.shiny-input-container{margin:0;width:auto !important;}
     .ctrlbar{display:flex;flex-wrap:wrap;align-items:center;gap:4px 26px;margin:6px 0 12px;}
     .ctrlbar>.shiny-input-container,.ctrlbar .checkbox{margin:0;width:auto !important;}
     .ctrlbar .fontctl .control-label{margin:0 0 1px;font-weight:400;}
     .ctrlbar .fontctl .form-group{margin:0;}
     .ctrlbar .fontctl .irs{margin:0;}
     .yearrow{position:relative;width:100%;max-width:1240px;height:26px;font-size:20px;font-weight:700;color:#cdd9e3;}
     .ylbl{position:absolute;white-space:nowrap;}"))),
  tags$h3("Extinction probability threshold controls"),
  div(class = "ctrlbar",
    checkboxInput("no_ctrl", "No control point", TRUE),
    checkboxInput("split_slope", "Split slopes", TRUE),
    checkboxInput("linear_y", "Linear y scale (10x ceiling)", TRUE),
    checkboxInput("lines_only", "Lines only", FALSE),
    checkboxInput("log_time", "Logarithmic time within each period", FALSE),
    div(class = "fontctl",
        sliderInput("font", "Font size (px)", min = 8, max = 28, value = 15, step = 1, width = "220px"))),
  div(class = "legendrow",
      checkboxInput("vis_haz",      HTML("<b style='color:#e8836f'>Extinction probability</b>"),          TRUE),
      checkboxInput("vis_surv",     HTML("<b style='color:#4fb8d6'>Survival probability</b>"),            TRUE),
      checkboxInput("vis_pop",      HTML("<b style='color:#e46ba6'>Population (potential / expected)</b>"), TRUE),
      checkboxInput("vis_cumyr",    HTML("<b style='color:#6fcf97'>Cumulative survived years</b>"),        FALSE),
      checkboxInput("vis_cumly",    HTML("<b style='color:#e8b04f'>Cumulative life-years</b>"),            FALSE),
      checkboxInput("vis_cumlives", HTML("<b style='color:#b98fe0'>Cumulative lives (&divide;75)</b>"),    FALSE)),
  div(class = "cmprow",
      tags$button(id = "baseCur",  class = "cmpbtn", "Baseline = current"),
      tags$button(id = "basePrev", class = "cmpbtn", "Baseline = previous"),
      actionButton("reset", "Reset", class = "cmpbtn")),
  year_row(),
  div(style = sprintf("position:relative; width:100%%; max-width:%dpx; aspect-ratio:%d/%d;", Wpx, Wpx, Hpx),
      plotOutput("plot", width = "100%", height = "100%"),
      HTML('<svg id="overlay" style="width:100%;height:100%;"></svg>')),
  year_row(),
  div(id = "popout", style = sprintf("position:relative; margin:12px 0; width:100%%; max-width:%dpx; min-height:140px; font-size:12px; color:#e46ba6;", Wpx)),
  div(id = "live", style = "margin:8px 0; font-family:monospace; color:#cdd9e3;"),
  tags$script(overlay_js)
)

## ---- server ---------------------------------------------------------------
server <- function(input, output, session) {

  # default hazard: rises early then declines over the long tail (5 checkpoints, 4 segments)
  hz_anchors <- c(1e-4, 7e-4, 3e-4, 8e-5, 3e-5)          # 2036 = 0.07%
  hz0 <- list(anchors = hz_anchors, thetaL = default_atheta(hz_anchors), thetaR = default_atheta(hz_anchors),
              fxc = ((1:4)-0.5)/4, cval = c((1e-4+7e-4)/2, (7e-4+3e-4)/2, (3e-4+8e-5)/2, (8e-5+3e-5)/2))
  sv0 <- list(anchors = c(0.98, 0.92, 0.75, 0.50, 0.30), thetaL = rep(0, 5), thetaR = rep(0, 5),
              fxc = ((1:4)-0.5)/4, cval = c(0.95, 0.83, 0.62, 0.40))

  rv <- reactiveValues(driver = "hazard", haz = hz0, surv = sv0)

  send_init   <- function() session$sendCustomMessage("init_points",
                    list(haz = rv$haz, surv = rv$surv, W = Wpx, H = Hpx))
  send_params <- function() session$sendCustomMessage("set_params",
                    list(points = !isTRUE(input$lines_only), useCtrl = !isTRUE(input$no_ctrl),
                         split = isTRUE(input$split_slope), linearY = isTRUE(input$linear_y),
                         logTime = isTRUE(input$log_time), font = input$font,
                         visHaz = isTRUE(input$vis_haz), visSurv = isTRUE(input$vis_surv),
                         visPop = isTRUE(input$vis_pop), visCumYr = isTRUE(input$vis_cumyr),
                         visCumLy = isTRUE(input$vis_cumly), visCumLives = isTRUE(input$vis_cumlives)))

  observeEvent(input$client_ready, { send_init(); send_params() })
  observeEvent(list(input$lines_only, input$no_ctrl, input$split_slope, input$linear_y, input$log_time, input$font,
                    input$vis_haz, input$vis_surv, input$vis_pop, input$vis_cumyr, input$vis_cumly, input$vis_cumlives),
               send_params())

  # cumulative life-years and cumulative lives are mutually exclusive
  observeEvent(input$vis_cumly,    if (isTRUE(input$vis_cumly))    updateCheckboxInput(session, "vis_cumlives", value = FALSE))
  observeEvent(input$vis_cumlives, if (isTRUE(input$vis_cumlives)) updateCheckboxInput(session, "vis_cumly",    value = FALSE))

  parse_set <- function(x) list(anchors = as.numeric(unlist(x$anchors)),
    thetaL = as.numeric(unlist(x$thetaL)), thetaR = as.numeric(unlist(x$thetaR)),
    fxc = as.numeric(unlist(x$fxc)), cval = as.numeric(unlist(x$cval)))

  observeEvent(input$curve_set, {
    cs <- input$curve_set; rv$driver <- cs$driver
    rv$haz <- parse_set(cs$haz); rv$surv <- parse_set(cs$surv)
  })

  observeEvent(input$reset, {
    rv$driver <- "hazard"; rv$haz <- hz0; rv$surv <- sv0
    session$sendCustomMessage("set_curve", list(driver = "hazard", haz = hz0, surv = sv0))
  })

  output$plot <- renderPlot({
    d <- compute_both(rv$haz, rv$surv, rv$driver, !isTRUE(input$no_ctrl), isTRUE(input$linear_y), isTRUE(input$log_time))
    p <- ggplot(d, aes(fx)) + geom_blank(aes(y = fy))   # base layer so an all-cumulative view still renders
    if (!isFALSE(input$vis_haz))                       # default (NULL before first send) shows it
      p <- p + geom_area(aes(y = fy), fill = "#e8836f", alpha = 0.15) +
               geom_line(aes(y = fy), colour = "#e8836f", linewidth = 0.6)
    if (!isFALSE(input$vis_surv))
      p <- p + geom_line(aes(y = surv), colour = "#4fb8d6", linewidth = 0.7)
    p +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
      theme_minimal(base_size = 13) +
      theme(axis.title = element_blank(), axis.text = element_blank(),
            axis.ticks = element_blank(), axis.ticks.length = unit(0, "pt"),
            plot.margin = margin(0, 0, 0, 0),
            panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
            panel.background = element_rect(fill = "#0b3552", colour = NA),
            plot.background  = element_rect(fill = "#0b3552", colour = NA))
  }, res = 96)
}

shinyApp(ui, server)
