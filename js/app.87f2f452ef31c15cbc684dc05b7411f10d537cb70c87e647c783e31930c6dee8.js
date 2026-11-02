"use strict";
(() => {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __require = /* @__PURE__ */ ((x) => typeof require !== "undefined" ? require : typeof Proxy !== "undefined" ? new Proxy(x, {
    get: (a2, b) => (typeof require !== "undefined" ? require : a2)[b]
  }) : x)(function(x) {
    if (typeof require !== "undefined") return require.apply(this, arguments);
    throw Error('Dynamic require of "' + x + '" is not supported');
  });
  var __commonJS = (cb, mod) => function __require2() {
    try {
      return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
    } catch (e2) {
      throw mod = 0, e2;
    }
  };
  var __copyProps = (to, from, except, desc) => {
    if (from && typeof from === "object" || typeof from === "function") {
      for (let key of __getOwnPropNames(from))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
    }
    return to;
  };
  var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
    // If the importer is in node compatibility mode or this is not an ESM
    // file that has been converted to a CommonJS file using a Babel-
    // compatible transform (i.e. "__esModule" has not been set), then set
    // "default" to the CommonJS "module.exports" for node compatibility.
    isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
    mod
  ));

  // node_modules/lazysizes/lazysizes.js
  var require_lazysizes = __commonJS({
    "node_modules/lazysizes/lazysizes.js"(exports, module) {
      (function(window2, factory) {
        var lazySizes2 = factory(window2, window2.document, Date);
        window2.lazySizes = lazySizes2;
        if (typeof module == "object" && module.exports) {
          module.exports = lazySizes2;
        }
      })(
        typeof window != "undefined" ? window : {},
        /**
         * import("./types/global")
         * @typedef { import("./types/lazysizes-config").LazySizesConfigPartial } LazySizesConfigPartial
         */
        function l2(window2, document2, Date2) {
          "use strict";
          var lazysizes, lazySizesCfg;
          (function() {
            var prop;
            var lazySizesDefaults = {
              lazyClass: "lazyload",
              loadedClass: "lazyloaded",
              loadingClass: "lazyloading",
              preloadClass: "lazypreload",
              errorClass: "lazyerror",
              //strictClass: 'lazystrict',
              autosizesClass: "lazyautosizes",
              fastLoadedClass: "ls-is-cached",
              iframeLoadMode: 0,
              srcAttr: "data-src",
              srcsetAttr: "data-srcset",
              sizesAttr: "data-sizes",
              //preloadAfterLoad: false,
              minSize: 40,
              customMedia: {},
              init: true,
              expFactor: 1.5,
              hFac: 0.8,
              loadMode: 2,
              loadHidden: true,
              ricTimeout: 0,
              throttleDelay: 125
            };
            lazySizesCfg = window2.lazySizesConfig || window2.lazysizesConfig || {};
            for (prop in lazySizesDefaults) {
              if (!(prop in lazySizesCfg)) {
                lazySizesCfg[prop] = lazySizesDefaults[prop];
              }
            }
          })();
          if (!document2 || !document2.getElementsByClassName) {
            return {
              init: function() {
              },
              /**
               * @type { LazySizesConfigPartial }
               */
              cfg: lazySizesCfg,
              /**
               * @type { true }
               */
              noSupport: true
            };
          }
          var docElem = document2.documentElement;
          var supportPicture = window2.HTMLPictureElement;
          var _addEventListener = "addEventListener";
          var _getAttribute = "getAttribute";
          var addEventListener = window2[_addEventListener].bind(window2);
          var setTimeout2 = window2.setTimeout;
          var requestAnimationFrame2 = window2.requestAnimationFrame || setTimeout2;
          var requestIdleCallback = window2.requestIdleCallback;
          var regPicture = /^picture$/i;
          var loadEvents = ["load", "error", "lazyincluded", "_lazyloaded"];
          var regClassCache = {};
          var forEach = Array.prototype.forEach;
          var hasClass = function(ele, cls) {
            if (!regClassCache[cls]) {
              regClassCache[cls] = new RegExp("(\\s|^)" + cls + "(\\s|$)");
            }
            return regClassCache[cls].test(ele[_getAttribute]("class") || "") && regClassCache[cls];
          };
          var addClass = function(ele, cls) {
            if (!hasClass(ele, cls)) {
              ele.setAttribute("class", (ele[_getAttribute]("class") || "").trim() + " " + cls);
            }
          };
          var removeClass = function(ele, cls) {
            var reg;
            if (reg = hasClass(ele, cls)) {
              ele.setAttribute("class", (ele[_getAttribute]("class") || "").replace(reg, " "));
            }
          };
          var addRemoveLoadEvents = function(dom, fn, add) {
            var action = add ? _addEventListener : "removeEventListener";
            if (add) {
              addRemoveLoadEvents(dom, fn);
            }
            loadEvents.forEach(function(evt) {
              dom[action](evt, fn);
            });
          };
          var triggerEvent = function(elem, name, detail, noBubbles, noCancelable) {
            var event = document2.createEvent("Event");
            if (!detail) {
              detail = {};
            }
            detail.instance = lazysizes;
            event.initEvent(name, !noBubbles, !noCancelable);
            event.detail = detail;
            elem.dispatchEvent(event);
            return event;
          };
          var updatePolyfill = function(el, full) {
            var polyfill;
            if (!supportPicture && (polyfill = window2.picturefill || lazySizesCfg.pf)) {
              if (full && full.src && !el[_getAttribute]("srcset")) {
                el.setAttribute("srcset", full.src);
              }
              polyfill({ reevaluate: true, elements: [el] });
            } else if (full && full.src) {
              el.src = full.src;
            }
          };
          var getCSS = function(elem, style) {
            return (getComputedStyle(elem, null) || {})[style];
          };
          var getWidth = function(elem, parent, width) {
            width = width || elem.offsetWidth;
            while (width < lazySizesCfg.minSize && parent && !elem._lazysizesWidth) {
              width = parent.offsetWidth;
              parent = parent.parentNode;
            }
            return width;
          };
          var rAF = (function() {
            var running, waiting;
            var firstFns = [];
            var secondFns = [];
            var fns = firstFns;
            var run = function() {
              var runFns = fns;
              fns = firstFns.length ? secondFns : firstFns;
              running = true;
              waiting = false;
              while (runFns.length) {
                runFns.shift()();
              }
              running = false;
            };
            var rafBatch = function(fn, queue) {
              if (running && !queue) {
                fn.apply(this, arguments);
              } else {
                fns.push(fn);
                if (!waiting) {
                  waiting = true;
                  (document2.hidden ? setTimeout2 : requestAnimationFrame2)(run);
                }
              }
            };
            rafBatch._lsFlush = run;
            return rafBatch;
          })();
          var rAFIt = function(fn, simple) {
            return simple ? function() {
              rAF(fn);
            } : function() {
              var that = this;
              var args = arguments;
              rAF(function() {
                fn.apply(that, args);
              });
            };
          };
          var throttle = function(fn) {
            var running;
            var lastTime = 0;
            var gDelay = lazySizesCfg.throttleDelay;
            var rICTimeout = lazySizesCfg.ricTimeout;
            var run = function() {
              running = false;
              lastTime = Date2.now();
              fn();
            };
            var idleCallback = requestIdleCallback && rICTimeout > 49 ? function() {
              requestIdleCallback(run, { timeout: rICTimeout });
              if (rICTimeout !== lazySizesCfg.ricTimeout) {
                rICTimeout = lazySizesCfg.ricTimeout;
              }
            } : rAFIt(function() {
              setTimeout2(run);
            }, true);
            return function(isPriority) {
              var delay;
              if (isPriority = isPriority === true) {
                rICTimeout = 33;
              }
              if (running) {
                return;
              }
              running = true;
              delay = gDelay - (Date2.now() - lastTime);
              if (delay < 0) {
                delay = 0;
              }
              if (isPriority || delay < 9) {
                idleCallback();
              } else {
                setTimeout2(idleCallback, delay);
              }
            };
          };
          var debounce = function(func) {
            var timeout, timestamp;
            var wait = 99;
            var run = function() {
              timeout = null;
              func();
            };
            var later = function() {
              var last = Date2.now() - timestamp;
              if (last < wait) {
                setTimeout2(later, wait - last);
              } else {
                (requestIdleCallback || run)(run);
              }
            };
            return function() {
              timestamp = Date2.now();
              if (!timeout) {
                timeout = setTimeout2(later, wait);
              }
            };
          };
          var loader = (function() {
            var preloadElems, isCompleted, resetPreloadingTimer, loadMode, started;
            var eLvW, elvH, eLtop, eLleft, eLright, eLbottom, isBodyHidden;
            var regImg = /^img$/i;
            var regIframe = /^iframe$/i;
            var supportScroll = "onscroll" in window2 && !/(gle|ing)bot/.test(navigator.userAgent);
            var shrinkExpand = 0;
            var currentExpand = 0;
            var isLoading = 0;
            var lowRuns = -1;
            var resetPreloading = function(e2) {
              isLoading--;
              if (!e2 || isLoading < 0 || !e2.target) {
                isLoading = 0;
              }
            };
            var isVisible = function(elem) {
              if (isBodyHidden == null) {
                isBodyHidden = getCSS(document2.body, "visibility") == "hidden";
              }
              return isBodyHidden || !(getCSS(elem.parentNode, "visibility") == "hidden" && getCSS(elem, "visibility") == "hidden");
            };
            var isNestedVisible = function(elem, elemExpand) {
              var outerRect;
              var parent = elem;
              var visible = isVisible(elem);
              eLtop -= elemExpand;
              eLbottom += elemExpand;
              eLleft -= elemExpand;
              eLright += elemExpand;
              while (visible && (parent = parent.offsetParent) && parent != document2.body && parent != docElem) {
                visible = (getCSS(parent, "opacity") || 1) > 0;
                if (visible && getCSS(parent, "overflow") != "visible") {
                  outerRect = parent.getBoundingClientRect();
                  visible = eLright > outerRect.left && eLleft < outerRect.right && eLbottom > outerRect.top - 1 && eLtop < outerRect.bottom + 1;
                }
              }
              return visible;
            };
            var checkElements = function() {
              var eLlen, i3, rect, autoLoadElem, loadedSomething, elemExpand, elemNegativeExpand, elemExpandVal, beforeExpandVal, defaultExpand, preloadExpand, hFac;
              var lazyloadElems = lazysizes.elements;
              if ((loadMode = lazySizesCfg.loadMode) && isLoading < 8 && (eLlen = lazyloadElems.length)) {
                i3 = 0;
                lowRuns++;
                for (; i3 < eLlen; i3++) {
                  if (!lazyloadElems[i3] || lazyloadElems[i3]._lazyRace) {
                    continue;
                  }
                  if (!supportScroll || lazysizes.prematureUnveil && lazysizes.prematureUnveil(lazyloadElems[i3])) {
                    unveilElement(lazyloadElems[i3]);
                    continue;
                  }
                  if (!(elemExpandVal = lazyloadElems[i3][_getAttribute]("data-expand")) || !(elemExpand = elemExpandVal * 1)) {
                    elemExpand = currentExpand;
                  }
                  if (!defaultExpand) {
                    defaultExpand = !lazySizesCfg.expand || lazySizesCfg.expand < 1 ? docElem.clientHeight > 500 && docElem.clientWidth > 500 ? 500 : 370 : lazySizesCfg.expand;
                    lazysizes._defEx = defaultExpand;
                    preloadExpand = defaultExpand * lazySizesCfg.expFactor;
                    hFac = lazySizesCfg.hFac;
                    isBodyHidden = null;
                    if (currentExpand < preloadExpand && isLoading < 1 && lowRuns > 2 && loadMode > 2 && !document2.hidden) {
                      currentExpand = preloadExpand;
                      lowRuns = 0;
                    } else if (loadMode > 1 && lowRuns > 1 && isLoading < 6) {
                      currentExpand = defaultExpand;
                    } else {
                      currentExpand = shrinkExpand;
                    }
                  }
                  if (beforeExpandVal !== elemExpand) {
                    eLvW = innerWidth + elemExpand * hFac;
                    elvH = innerHeight + elemExpand;
                    elemNegativeExpand = elemExpand * -1;
                    beforeExpandVal = elemExpand;
                  }
                  rect = lazyloadElems[i3].getBoundingClientRect();
                  if ((eLbottom = rect.bottom) >= elemNegativeExpand && (eLtop = rect.top) <= elvH && (eLright = rect.right) >= elemNegativeExpand * hFac && (eLleft = rect.left) <= eLvW && (eLbottom || eLright || eLleft || eLtop) && (lazySizesCfg.loadHidden || isVisible(lazyloadElems[i3])) && (isCompleted && isLoading < 3 && !elemExpandVal && (loadMode < 3 || lowRuns < 4) || isNestedVisible(lazyloadElems[i3], elemExpand))) {
                    unveilElement(lazyloadElems[i3]);
                    loadedSomething = true;
                    if (isLoading > 9) {
                      break;
                    }
                  } else if (!loadedSomething && isCompleted && !autoLoadElem && isLoading < 4 && lowRuns < 4 && loadMode > 2 && (preloadElems[0] || lazySizesCfg.preloadAfterLoad) && (preloadElems[0] || !elemExpandVal && (eLbottom || eLright || eLleft || eLtop || lazyloadElems[i3][_getAttribute](lazySizesCfg.sizesAttr) != "auto"))) {
                    autoLoadElem = preloadElems[0] || lazyloadElems[i3];
                  }
                }
                if (autoLoadElem && !loadedSomething) {
                  unveilElement(autoLoadElem);
                }
              }
            };
            var throttledCheckElements = throttle(checkElements);
            var switchLoadingClass = function(e2) {
              var elem = e2.target;
              if (elem._lazyCache) {
                delete elem._lazyCache;
                return;
              }
              resetPreloading(e2);
              addClass(elem, lazySizesCfg.loadedClass);
              removeClass(elem, lazySizesCfg.loadingClass);
              addRemoveLoadEvents(elem, rafSwitchLoadingClass);
              triggerEvent(elem, "lazyloaded");
            };
            var rafedSwitchLoadingClass = rAFIt(switchLoadingClass);
            var rafSwitchLoadingClass = function(e2) {
              rafedSwitchLoadingClass({ target: e2.target });
            };
            var changeIframeSrc = function(elem, src) {
              var loadMode2 = elem.getAttribute("data-load-mode") || lazySizesCfg.iframeLoadMode;
              if (loadMode2 == 0) {
                elem.contentWindow.location.replace(src);
              } else if (loadMode2 == 1) {
                elem.src = src;
              }
            };
            var handleSources = function(source) {
              var customMedia;
              var sourceSrcset = source[_getAttribute](lazySizesCfg.srcsetAttr);
              if (customMedia = lazySizesCfg.customMedia[source[_getAttribute]("data-media") || source[_getAttribute]("media")]) {
                source.setAttribute("media", customMedia);
              }
              if (sourceSrcset) {
                source.setAttribute("srcset", sourceSrcset);
              }
            };
            var lazyUnveil = rAFIt(function(elem, detail, isAuto, sizes, isImg) {
              var src, srcset, parent, isPicture, event, firesLoad;
              if (!(event = triggerEvent(elem, "lazybeforeunveil", detail)).defaultPrevented) {
                if (sizes) {
                  if (isAuto) {
                    addClass(elem, lazySizesCfg.autosizesClass);
                  } else {
                    elem.setAttribute("sizes", sizes);
                  }
                }
                srcset = elem[_getAttribute](lazySizesCfg.srcsetAttr);
                src = elem[_getAttribute](lazySizesCfg.srcAttr);
                if (isImg) {
                  parent = elem.parentNode;
                  isPicture = parent && regPicture.test(parent.nodeName || "");
                }
                firesLoad = detail.firesLoad || "src" in elem && (srcset || src || isPicture);
                event = { target: elem };
                addClass(elem, lazySizesCfg.loadingClass);
                if (firesLoad) {
                  clearTimeout(resetPreloadingTimer);
                  resetPreloadingTimer = setTimeout2(resetPreloading, 2500);
                  addRemoveLoadEvents(elem, rafSwitchLoadingClass, true);
                }
                if (isPicture) {
                  forEach.call(parent.getElementsByTagName("source"), handleSources);
                }
                if (srcset) {
                  elem.setAttribute("srcset", srcset);
                } else if (src && !isPicture) {
                  if (regIframe.test(elem.nodeName)) {
                    changeIframeSrc(elem, src);
                  } else {
                    elem.src = src;
                  }
                }
                if (isImg && (srcset || isPicture)) {
                  updatePolyfill(elem, { src });
                }
              }
              if (elem._lazyRace) {
                delete elem._lazyRace;
              }
              removeClass(elem, lazySizesCfg.lazyClass);
              rAF(function() {
                var isLoaded = elem.complete && elem.naturalWidth > 1;
                if (!firesLoad || isLoaded) {
                  if (isLoaded) {
                    addClass(elem, lazySizesCfg.fastLoadedClass);
                  }
                  switchLoadingClass(event);
                  elem._lazyCache = true;
                  setTimeout2(function() {
                    if ("_lazyCache" in elem) {
                      delete elem._lazyCache;
                    }
                  }, 9);
                }
                if (elem.loading == "lazy") {
                  isLoading--;
                }
              }, true);
            });
            var unveilElement = function(elem) {
              if (elem._lazyRace) {
                return;
              }
              var detail;
              var isImg = regImg.test(elem.nodeName);
              var sizes = isImg && (elem[_getAttribute](lazySizesCfg.sizesAttr) || elem[_getAttribute]("sizes"));
              var isAuto = sizes == "auto";
              if ((isAuto || !isCompleted) && isImg && (elem[_getAttribute]("src") || elem.srcset) && !elem.complete && !hasClass(elem, lazySizesCfg.errorClass) && hasClass(elem, lazySizesCfg.lazyClass)) {
                return;
              }
              detail = triggerEvent(elem, "lazyunveilread").detail;
              if (isAuto) {
                autoSizer.updateElem(elem, true, elem.offsetWidth);
              }
              elem._lazyRace = true;
              isLoading++;
              lazyUnveil(elem, detail, isAuto, sizes, isImg);
            };
            var afterScroll = debounce(function() {
              lazySizesCfg.loadMode = 3;
              throttledCheckElements();
            });
            var altLoadmodeScrollListner = function() {
              if (lazySizesCfg.loadMode == 3) {
                lazySizesCfg.loadMode = 2;
              }
              afterScroll();
            };
            var onload = function() {
              if (isCompleted) {
                return;
              }
              if (Date2.now() - started < 999) {
                setTimeout2(onload, 999);
                return;
              }
              isCompleted = true;
              lazySizesCfg.loadMode = 3;
              throttledCheckElements();
              addEventListener("scroll", altLoadmodeScrollListner, true);
            };
            return {
              _: function() {
                started = Date2.now();
                lazysizes.elements = document2.getElementsByClassName(lazySizesCfg.lazyClass);
                preloadElems = document2.getElementsByClassName(lazySizesCfg.lazyClass + " " + lazySizesCfg.preloadClass);
                addEventListener("scroll", throttledCheckElements, true);
                addEventListener("resize", throttledCheckElements, true);
                addEventListener("pageshow", function(e2) {
                  if (e2.persisted) {
                    var loadingElements = document2.querySelectorAll("." + lazySizesCfg.loadingClass);
                    if (loadingElements.length && loadingElements.forEach) {
                      requestAnimationFrame2(function() {
                        loadingElements.forEach(function(img) {
                          if (img.complete) {
                            unveilElement(img);
                          }
                        });
                      });
                    }
                  }
                });
                if (window2.MutationObserver) {
                  new MutationObserver(throttledCheckElements).observe(docElem, { childList: true, subtree: true, attributes: true });
                } else {
                  docElem[_addEventListener]("DOMNodeInserted", throttledCheckElements, true);
                  docElem[_addEventListener]("DOMAttrModified", throttledCheckElements, true);
                  setInterval(throttledCheckElements, 999);
                }
                addEventListener("hashchange", throttledCheckElements, true);
                ["focus", "mouseover", "click", "load", "transitionend", "animationend"].forEach(function(name) {
                  document2[_addEventListener](name, throttledCheckElements, true);
                });
                if (/d$|^c/.test(document2.readyState)) {
                  onload();
                } else {
                  addEventListener("load", onload);
                  document2[_addEventListener]("DOMContentLoaded", throttledCheckElements);
                  setTimeout2(onload, 2e4);
                }
                if (lazysizes.elements.length) {
                  checkElements();
                  rAF._lsFlush();
                } else {
                  throttledCheckElements();
                }
              },
              checkElems: throttledCheckElements,
              unveil: unveilElement,
              _aLSL: altLoadmodeScrollListner
            };
          })();
          var autoSizer = (function() {
            var autosizesElems;
            var sizeElement = rAFIt(function(elem, parent, event, width) {
              var sources, i3, len;
              elem._lazysizesWidth = width;
              width += "px";
              elem.setAttribute("sizes", width);
              if (regPicture.test(parent.nodeName || "")) {
                sources = parent.getElementsByTagName("source");
                for (i3 = 0, len = sources.length; i3 < len; i3++) {
                  sources[i3].setAttribute("sizes", width);
                }
              }
              if (!event.detail.dataAttr) {
                updatePolyfill(elem, event.detail);
              }
            });
            var getSizeElement = function(elem, dataAttr, width) {
              var event;
              var parent = elem.parentNode;
              if (parent) {
                width = getWidth(elem, parent, width);
                event = triggerEvent(elem, "lazybeforesizes", { width, dataAttr: !!dataAttr });
                if (!event.defaultPrevented) {
                  width = event.detail.width;
                  if (width && width !== elem._lazysizesWidth) {
                    sizeElement(elem, parent, event, width);
                  }
                }
              }
            };
            var updateElementsSizes = function() {
              var i3;
              var len = autosizesElems.length;
              if (len) {
                i3 = 0;
                for (; i3 < len; i3++) {
                  getSizeElement(autosizesElems[i3]);
                }
              }
            };
            var debouncedUpdateElementsSizes = debounce(updateElementsSizes);
            return {
              _: function() {
                autosizesElems = document2.getElementsByClassName(lazySizesCfg.autosizesClass);
                addEventListener("resize", debouncedUpdateElementsSizes);
              },
              checkElems: debouncedUpdateElementsSizes,
              updateElem: getSizeElement
            };
          })();
          var init = function() {
            if (!init.i && document2.getElementsByClassName) {
              init.i = true;
              autoSizer._();
              loader._();
            }
          };
          setTimeout2(function() {
            if (lazySizesCfg.init) {
              init();
            }
          });
          lazysizes = {
            /**
             * @type { LazySizesConfigPartial }
             */
            cfg: lazySizesCfg,
            autoSizer,
            loader,
            init,
            uP: updatePolyfill,
            aC: addClass,
            rC: removeClass,
            hC: hasClass,
            fire: triggerEvent,
            gW: getWidth,
            rAF
          };
          return lazysizes;
        }
      );
    }
  });

  // node_modules/lazysizes/plugins/native-loading/ls.native-loading.js
  var require_ls_native_loading = __commonJS({
    "node_modules/lazysizes/plugins/native-loading/ls.native-loading.js"(exports, module) {
      (function(window2, factory) {
        var globalInstall = function() {
          factory(window2.lazySizes);
          window2.removeEventListener("lazyunveilread", globalInstall, true);
        };
        factory = factory.bind(null, window2, window2.document);
        if (typeof module == "object" && module.exports) {
          factory(require_lazysizes());
        } else if (typeof define == "function" && define.amd) {
          define(["lazysizes"], factory);
        } else if (window2.lazySizes) {
          globalInstall();
        } else {
          window2.addEventListener("lazyunveilread", globalInstall, true);
        }
      })(window, function(window2, document2, lazySizes2) {
        "use strict";
        var imgSupport = "loading" in HTMLImageElement.prototype;
        var iframeSupport = "loading" in HTMLIFrameElement.prototype;
        var isConfigSet = false;
        var oldPrematureUnveil = lazySizes2.prematureUnveil;
        var cfg = lazySizes2.cfg;
        var listenerMap = {
          focus: 1,
          mouseover: 1,
          click: 1,
          load: 1,
          transitionend: 1,
          animationend: 1,
          scroll: 1,
          resize: 1
        };
        if (!cfg.nativeLoading) {
          cfg.nativeLoading = {};
        }
        if (!window2.addEventListener || !window2.MutationObserver || !imgSupport && !iframeSupport) {
          return;
        }
        function disableEvents() {
          var loader = lazySizes2.loader;
          var throttledCheckElements = loader.checkElems;
          var removeALSL = function() {
            setTimeout(function() {
              window2.removeEventListener("scroll", loader._aLSL, true);
            }, 1e3);
          };
          var currentListenerMap = typeof cfg.nativeLoading.disableListeners == "object" ? cfg.nativeLoading.disableListeners : listenerMap;
          if (currentListenerMap.scroll) {
            window2.addEventListener("load", removeALSL);
            removeALSL();
            window2.removeEventListener("scroll", throttledCheckElements, true);
          }
          if (currentListenerMap.resize) {
            window2.removeEventListener("resize", throttledCheckElements, true);
          }
          Object.keys(currentListenerMap).forEach(function(name) {
            if (currentListenerMap[name]) {
              document2.removeEventListener(name, throttledCheckElements, true);
            }
          });
        }
        function runConfig() {
          if (isConfigSet) {
            return;
          }
          isConfigSet = true;
          if (imgSupport && iframeSupport && cfg.nativeLoading.disableListeners) {
            if (cfg.nativeLoading.disableListeners === true) {
              cfg.nativeLoading.setLoadingAttribute = true;
            }
            disableEvents();
          }
          if (cfg.nativeLoading.setLoadingAttribute) {
            window2.addEventListener("lazybeforeunveil", function(e2) {
              var element = e2.target;
              if ("loading" in element && !element.getAttribute("loading")) {
                element.setAttribute("loading", "lazy");
              }
            }, true);
          }
        }
        lazySizes2.prematureUnveil = function prematureUnveil(element) {
          if (!isConfigSet) {
            runConfig();
          }
          if ("loading" in element && (cfg.nativeLoading.setLoadingAttribute || element.getAttribute("loading")) && (element.getAttribute("data-sizes") != "auto" || element.offsetWidth)) {
            return true;
          }
          if (oldPrematureUnveil) {
            return oldPrematureUnveil(element);
          }
        };
      });
    }
  });

  // node_modules/clipboard/dist/clipboard.js
  var require_clipboard = __commonJS({
    "node_modules/clipboard/dist/clipboard.js"(exports, module) {
      (function webpackUniversalModuleDefinition(root, factory) {
        if (typeof exports === "object" && typeof module === "object")
          module.exports = factory();
        else if (typeof define === "function" && define.amd)
          define([], factory);
        else if (typeof exports === "object")
          exports["ClipboardJS"] = factory();
        else
          root["ClipboardJS"] = factory();
      })(exports, function() {
        return (
          /******/
          (function() {
            var __webpack_modules__ = {
              /***/
              686: (
                /***/
                (function(__unused_webpack_module, __webpack_exports__, __webpack_require__2) {
                  "use strict";
                  __webpack_require__2.d(__webpack_exports__, {
                    "default": function() {
                      return (
                        /* binding */
                        clipboard
                      );
                    }
                  });
                  var tiny_emitter = __webpack_require__2(279);
                  var tiny_emitter_default = /* @__PURE__ */ __webpack_require__2.n(tiny_emitter);
                  var listen = __webpack_require__2(370);
                  var listen_default = /* @__PURE__ */ __webpack_require__2.n(listen);
                  var src_select = __webpack_require__2(817);
                  var select_default = /* @__PURE__ */ __webpack_require__2.n(src_select);
                  ;
                  function command(type) {
                    try {
                      return document.execCommand(type);
                    } catch (err) {
                      return false;
                    }
                  }
                  ;
                  var ClipboardActionCut = function ClipboardActionCut2(target) {
                    var selectedText = select_default()(target);
                    command("cut");
                    return selectedText;
                  };
                  var actions_cut = ClipboardActionCut;
                  ;
                  function createFakeElement(value) {
                    var isRTL = document.documentElement.getAttribute("dir") === "rtl";
                    var fakeElement = document.createElement("textarea");
                    fakeElement.style.fontSize = "12pt";
                    fakeElement.style.border = "0";
                    fakeElement.style.padding = "0";
                    fakeElement.style.margin = "0";
                    fakeElement.style.position = "absolute";
                    fakeElement.style[isRTL ? "right" : "left"] = "-9999px";
                    var yPosition = window.pageYOffset || document.documentElement.scrollTop;
                    fakeElement.style.top = "".concat(yPosition, "px");
                    fakeElement.setAttribute("readonly", "");
                    fakeElement.value = value;
                    return fakeElement;
                  }
                  ;
                  var fakeCopyAction = function fakeCopyAction2(value, options) {
                    var fakeElement = createFakeElement(value);
                    options.container.appendChild(fakeElement);
                    var selectedText = select_default()(fakeElement);
                    command("copy");
                    fakeElement.remove();
                    return selectedText;
                  };
                  var ClipboardActionCopy = function ClipboardActionCopy2(target) {
                    var options = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : {
                      container: document.body
                    };
                    var selectedText = "";
                    if (typeof target === "string") {
                      selectedText = fakeCopyAction(target, options);
                    } else if (target instanceof HTMLInputElement && !["text", "search", "url", "tel", "password"].includes(target === null || target === void 0 ? void 0 : target.type)) {
                      selectedText = fakeCopyAction(target.value, options);
                    } else {
                      selectedText = select_default()(target);
                      command("copy");
                    }
                    return selectedText;
                  };
                  var actions_copy = ClipboardActionCopy;
                  ;
                  function _typeof(obj) {
                    "@babel/helpers - typeof";
                    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
                      _typeof = function _typeof2(obj2) {
                        return typeof obj2;
                      };
                    } else {
                      _typeof = function _typeof2(obj2) {
                        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
                      };
                    }
                    return _typeof(obj);
                  }
                  var ClipboardActionDefault = function ClipboardActionDefault2() {
                    var options = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {};
                    var _options$action = options.action, action = _options$action === void 0 ? "copy" : _options$action, container = options.container, target = options.target, text = options.text;
                    if (action !== "copy" && action !== "cut") {
                      throw new Error('Invalid "action" value, use either "copy" or "cut"');
                    }
                    if (target !== void 0) {
                      if (target && _typeof(target) === "object" && target.nodeType === 1) {
                        if (action === "copy" && target.hasAttribute("disabled")) {
                          throw new Error('Invalid "target" attribute. Please use "readonly" instead of "disabled" attribute');
                        }
                        if (action === "cut" && (target.hasAttribute("readonly") || target.hasAttribute("disabled"))) {
                          throw new Error(`Invalid "target" attribute. You can't cut text from elements with "readonly" or "disabled" attributes`);
                        }
                      } else {
                        throw new Error('Invalid "target" value, use a valid Element');
                      }
                    }
                    if (text) {
                      return actions_copy(text, {
                        container
                      });
                    }
                    if (target) {
                      return action === "cut" ? actions_cut(target) : actions_copy(target, {
                        container
                      });
                    }
                  };
                  var actions_default = ClipboardActionDefault;
                  ;
                  function clipboard_typeof(obj) {
                    "@babel/helpers - typeof";
                    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
                      clipboard_typeof = function _typeof2(obj2) {
                        return typeof obj2;
                      };
                    } else {
                      clipboard_typeof = function _typeof2(obj2) {
                        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
                      };
                    }
                    return clipboard_typeof(obj);
                  }
                  function _classCallCheck(instance, Constructor) {
                    if (!(instance instanceof Constructor)) {
                      throw new TypeError("Cannot call a class as a function");
                    }
                  }
                  function _defineProperties(target, props) {
                    for (var i3 = 0; i3 < props.length; i3++) {
                      var descriptor = props[i3];
                      descriptor.enumerable = descriptor.enumerable || false;
                      descriptor.configurable = true;
                      if ("value" in descriptor) descriptor.writable = true;
                      Object.defineProperty(target, descriptor.key, descriptor);
                    }
                  }
                  function _createClass(Constructor, protoProps, staticProps) {
                    if (protoProps) _defineProperties(Constructor.prototype, protoProps);
                    if (staticProps) _defineProperties(Constructor, staticProps);
                    return Constructor;
                  }
                  function _inherits(subClass, superClass) {
                    if (typeof superClass !== "function" && superClass !== null) {
                      throw new TypeError("Super expression must either be null or a function");
                    }
                    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
                    if (superClass) _setPrototypeOf(subClass, superClass);
                  }
                  function _setPrototypeOf(o2, p) {
                    _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf2(o3, p2) {
                      o3.__proto__ = p2;
                      return o3;
                    };
                    return _setPrototypeOf(o2, p);
                  }
                  function _createSuper(Derived) {
                    var hasNativeReflectConstruct = _isNativeReflectConstruct();
                    return function _createSuperInternal() {
                      var Super = _getPrototypeOf(Derived), result;
                      if (hasNativeReflectConstruct) {
                        var NewTarget = _getPrototypeOf(this).constructor;
                        result = Reflect.construct(Super, arguments, NewTarget);
                      } else {
                        result = Super.apply(this, arguments);
                      }
                      return _possibleConstructorReturn(this, result);
                    };
                  }
                  function _possibleConstructorReturn(self2, call) {
                    if (call && (clipboard_typeof(call) === "object" || typeof call === "function")) {
                      return call;
                    }
                    return _assertThisInitialized(self2);
                  }
                  function _assertThisInitialized(self2) {
                    if (self2 === void 0) {
                      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
                    }
                    return self2;
                  }
                  function _isNativeReflectConstruct() {
                    if (typeof Reflect === "undefined" || !Reflect.construct) return false;
                    if (Reflect.construct.sham) return false;
                    if (typeof Proxy === "function") return true;
                    try {
                      Date.prototype.toString.call(Reflect.construct(Date, [], function() {
                      }));
                      return true;
                    } catch (e2) {
                      return false;
                    }
                  }
                  function _getPrototypeOf(o2) {
                    _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf2(o3) {
                      return o3.__proto__ || Object.getPrototypeOf(o3);
                    };
                    return _getPrototypeOf(o2);
                  }
                  function getAttributeValue(suffix, element) {
                    var attribute = "data-clipboard-".concat(suffix);
                    if (!element.hasAttribute(attribute)) {
                      return;
                    }
                    return element.getAttribute(attribute);
                  }
                  var Clipboard2 = /* @__PURE__ */ (function(_Emitter) {
                    _inherits(Clipboard3, _Emitter);
                    var _super = _createSuper(Clipboard3);
                    function Clipboard3(trigger, options) {
                      var _this;
                      _classCallCheck(this, Clipboard3);
                      _this = _super.call(this);
                      _this.resolveOptions(options);
                      _this.listenClick(trigger);
                      return _this;
                    }
                    _createClass(Clipboard3, [{
                      key: "resolveOptions",
                      value: function resolveOptions() {
                        var options = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {};
                        this.action = typeof options.action === "function" ? options.action : this.defaultAction;
                        this.target = typeof options.target === "function" ? options.target : this.defaultTarget;
                        this.text = typeof options.text === "function" ? options.text : this.defaultText;
                        this.container = clipboard_typeof(options.container) === "object" ? options.container : document.body;
                      }
                      /**
                       * Adds a click event listener to the passed trigger.
                       * @param {String|HTMLElement|HTMLCollection|NodeList} trigger
                       */
                    }, {
                      key: "listenClick",
                      value: function listenClick(trigger) {
                        var _this2 = this;
                        this.listener = listen_default()(trigger, "click", function(e2) {
                          return _this2.onClick(e2);
                        });
                      }
                      /**
                       * Defines a new `ClipboardAction` on each click event.
                       * @param {Event} e
                       */
                    }, {
                      key: "onClick",
                      value: function onClick(e2) {
                        var trigger = e2.delegateTarget || e2.currentTarget;
                        var action = this.action(trigger) || "copy";
                        var text = actions_default({
                          action,
                          container: this.container,
                          target: this.target(trigger),
                          text: this.text(trigger)
                        });
                        this.emit(text ? "success" : "error", {
                          action,
                          text,
                          trigger,
                          clearSelection: function clearSelection() {
                            if (trigger) {
                              trigger.focus();
                            }
                            window.getSelection().removeAllRanges();
                          }
                        });
                      }
                      /**
                       * Default `action` lookup function.
                       * @param {Element} trigger
                       */
                    }, {
                      key: "defaultAction",
                      value: function defaultAction(trigger) {
                        return getAttributeValue("action", trigger);
                      }
                      /**
                       * Default `target` lookup function.
                       * @param {Element} trigger
                       */
                    }, {
                      key: "defaultTarget",
                      value: function defaultTarget(trigger) {
                        var selector = getAttributeValue("target", trigger);
                        if (selector) {
                          return document.querySelector(selector);
                        }
                      }
                      /**
                       * Allow fire programmatically a copy action
                       * @param {String|HTMLElement} target
                       * @param {Object} options
                       * @returns Text copied.
                       */
                    }, {
                      key: "defaultText",
                      /**
                       * Default `text` lookup function.
                       * @param {Element} trigger
                       */
                      value: function defaultText(trigger) {
                        return getAttributeValue("text", trigger);
                      }
                      /**
                       * Destroy lifecycle.
                       */
                    }, {
                      key: "destroy",
                      value: function destroy() {
                        this.listener.destroy();
                      }
                    }], [{
                      key: "copy",
                      value: function copy(target) {
                        var options = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : {
                          container: document.body
                        };
                        return actions_copy(target, options);
                      }
                      /**
                       * Allow fire programmatically a cut action
                       * @param {String|HTMLElement} target
                       * @returns Text cutted.
                       */
                    }, {
                      key: "cut",
                      value: function cut(target) {
                        return actions_cut(target);
                      }
                      /**
                       * Returns the support of the given action, or all actions if no action is
                       * given.
                       * @param {String} [action]
                       */
                    }, {
                      key: "isSupported",
                      value: function isSupported() {
                        var action = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : ["copy", "cut"];
                        var actions = typeof action === "string" ? [action] : action;
                        var support = !!document.queryCommandSupported;
                        actions.forEach(function(action2) {
                          support = support && !!document.queryCommandSupported(action2);
                        });
                        return support;
                      }
                    }]);
                    return Clipboard3;
                  })(tiny_emitter_default());
                  var clipboard = Clipboard2;
                })
              ),
              /***/
              828: (
                /***/
                (function(module2) {
                  var DOCUMENT_NODE_TYPE = 9;
                  if (typeof Element !== "undefined" && !Element.prototype.matches) {
                    var proto = Element.prototype;
                    proto.matches = proto.matchesSelector || proto.mozMatchesSelector || proto.msMatchesSelector || proto.oMatchesSelector || proto.webkitMatchesSelector;
                  }
                  function closest(element, selector) {
                    while (element && element.nodeType !== DOCUMENT_NODE_TYPE) {
                      if (typeof element.matches === "function" && element.matches(selector)) {
                        return element;
                      }
                      element = element.parentNode;
                    }
                  }
                  module2.exports = closest;
                })
              ),
              /***/
              438: (
                /***/
                (function(module2, __unused_webpack_exports, __webpack_require__2) {
                  var closest = __webpack_require__2(828);
                  function _delegate(element, selector, type, callback, useCapture) {
                    var listenerFn = listener.apply(this, arguments);
                    element.addEventListener(type, listenerFn, useCapture);
                    return {
                      destroy: function() {
                        element.removeEventListener(type, listenerFn, useCapture);
                      }
                    };
                  }
                  function delegate(elements, selector, type, callback, useCapture) {
                    if (typeof elements.addEventListener === "function") {
                      return _delegate.apply(null, arguments);
                    }
                    if (typeof type === "function") {
                      return _delegate.bind(null, document).apply(null, arguments);
                    }
                    if (typeof elements === "string") {
                      elements = document.querySelectorAll(elements);
                    }
                    return Array.prototype.map.call(elements, function(element) {
                      return _delegate(element, selector, type, callback, useCapture);
                    });
                  }
                  function listener(element, selector, type, callback) {
                    return function(e2) {
                      e2.delegateTarget = closest(e2.target, selector);
                      if (e2.delegateTarget) {
                        callback.call(element, e2);
                      }
                    };
                  }
                  module2.exports = delegate;
                })
              ),
              /***/
              879: (
                /***/
                (function(__unused_webpack_module, exports2) {
                  exports2.node = function(value) {
                    return value !== void 0 && value instanceof HTMLElement && value.nodeType === 1;
                  };
                  exports2.nodeList = function(value) {
                    var type = Object.prototype.toString.call(value);
                    return value !== void 0 && (type === "[object NodeList]" || type === "[object HTMLCollection]") && "length" in value && (value.length === 0 || exports2.node(value[0]));
                  };
                  exports2.string = function(value) {
                    return typeof value === "string" || value instanceof String;
                  };
                  exports2.fn = function(value) {
                    var type = Object.prototype.toString.call(value);
                    return type === "[object Function]";
                  };
                })
              ),
              /***/
              370: (
                /***/
                (function(module2, __unused_webpack_exports, __webpack_require__2) {
                  var is = __webpack_require__2(879);
                  var delegate = __webpack_require__2(438);
                  function listen(target, type, callback) {
                    if (!target && !type && !callback) {
                      throw new Error("Missing required arguments");
                    }
                    if (!is.string(type)) {
                      throw new TypeError("Second argument must be a String");
                    }
                    if (!is.fn(callback)) {
                      throw new TypeError("Third argument must be a Function");
                    }
                    if (is.node(target)) {
                      return listenNode(target, type, callback);
                    } else if (is.nodeList(target)) {
                      return listenNodeList(target, type, callback);
                    } else if (is.string(target)) {
                      return listenSelector(target, type, callback);
                    } else {
                      throw new TypeError("First argument must be a String, HTMLElement, HTMLCollection, or NodeList");
                    }
                  }
                  function listenNode(node, type, callback) {
                    node.addEventListener(type, callback);
                    return {
                      destroy: function() {
                        node.removeEventListener(type, callback);
                      }
                    };
                  }
                  function listenNodeList(nodeList, type, callback) {
                    Array.prototype.forEach.call(nodeList, function(node) {
                      node.addEventListener(type, callback);
                    });
                    return {
                      destroy: function() {
                        Array.prototype.forEach.call(nodeList, function(node) {
                          node.removeEventListener(type, callback);
                        });
                      }
                    };
                  }
                  function listenSelector(selector, type, callback) {
                    return delegate(document.body, selector, type, callback);
                  }
                  module2.exports = listen;
                })
              ),
              /***/
              817: (
                /***/
                (function(module2) {
                  function select(element) {
                    var selectedText;
                    if (element.nodeName === "SELECT") {
                      element.focus();
                      selectedText = element.value;
                    } else if (element.nodeName === "INPUT" || element.nodeName === "TEXTAREA") {
                      var isReadOnly = element.hasAttribute("readonly");
                      if (!isReadOnly) {
                        element.setAttribute("readonly", "");
                      }
                      element.select();
                      element.setSelectionRange(0, element.value.length);
                      if (!isReadOnly) {
                        element.removeAttribute("readonly");
                      }
                      selectedText = element.value;
                    } else {
                      if (element.hasAttribute("contenteditable")) {
                        element.focus();
                      }
                      var selection = window.getSelection();
                      var range = document.createRange();
                      range.selectNodeContents(element);
                      selection.removeAllRanges();
                      selection.addRange(range);
                      selectedText = selection.toString();
                    }
                    return selectedText;
                  }
                  module2.exports = select;
                })
              ),
              /***/
              279: (
                /***/
                (function(module2) {
                  function E() {
                  }
                  E.prototype = {
                    on: function(name, callback, ctx) {
                      var e2 = this.e || (this.e = {});
                      (e2[name] || (e2[name] = [])).push({
                        fn: callback,
                        ctx
                      });
                      return this;
                    },
                    once: function(name, callback, ctx) {
                      var self2 = this;
                      function listener() {
                        self2.off(name, listener);
                        callback.apply(ctx, arguments);
                      }
                      ;
                      listener._ = callback;
                      return this.on(name, listener, ctx);
                    },
                    emit: function(name) {
                      var data = [].slice.call(arguments, 1);
                      var evtArr = ((this.e || (this.e = {}))[name] || []).slice();
                      var i3 = 0;
                      var len = evtArr.length;
                      for (i3; i3 < len; i3++) {
                        evtArr[i3].fn.apply(evtArr[i3].ctx, data);
                      }
                      return this;
                    },
                    off: function(name, callback) {
                      var e2 = this.e || (this.e = {});
                      var evts = e2[name];
                      var liveEvents = [];
                      if (evts && callback) {
                        for (var i3 = 0, len = evts.length; i3 < len; i3++) {
                          if (evts[i3].fn !== callback && evts[i3].fn._ !== callback)
                            liveEvents.push(evts[i3]);
                        }
                      }
                      liveEvents.length ? e2[name] = liveEvents : delete e2[name];
                      return this;
                    }
                  };
                  module2.exports = E;
                  module2.exports.TinyEmitter = E;
                })
              )
              /******/
            };
            var __webpack_module_cache__ = {};
            function __webpack_require__(moduleId) {
              if (__webpack_module_cache__[moduleId]) {
                return __webpack_module_cache__[moduleId].exports;
              }
              var module2 = __webpack_module_cache__[moduleId] = {
                /******/
                // no module.id needed
                /******/
                // no module.loaded needed
                /******/
                exports: {}
                /******/
              };
              __webpack_modules__[moduleId](module2, module2.exports, __webpack_require__);
              return module2.exports;
            }
            !(function() {
              __webpack_require__.n = function(module2) {
                var getter = module2 && module2.__esModule ? (
                  /******/
                  function() {
                    return module2["default"];
                  }
                ) : (
                  /******/
                  function() {
                    return module2;
                  }
                );
                __webpack_require__.d(getter, { a: getter });
                return getter;
              };
            })();
            !(function() {
              __webpack_require__.d = function(exports2, definition) {
                for (var key in definition) {
                  if (__webpack_require__.o(definition, key) && !__webpack_require__.o(exports2, key)) {
                    Object.defineProperty(exports2, key, { enumerable: true, get: definition[key] });
                  }
                }
              };
            })();
            !(function() {
              __webpack_require__.o = function(obj, prop) {
                return Object.prototype.hasOwnProperty.call(obj, prop);
              };
            })();
            return __webpack_require__(686);
          })().default
        );
      });
    }
  });

  // node_modules/basiclightbox/dist/basicLightbox.min.js
  var require_basicLightbox_min = __commonJS({
    "node_modules/basiclightbox/dist/basicLightbox.min.js"(exports, module) {
      !(function(e2) {
        if ("object" == typeof exports && "undefined" != typeof module) module.exports = e2();
        else if ("function" == typeof define && define.amd) define([], e2);
        else {
          ("undefined" != typeof window ? window : "undefined" != typeof global ? global : "undefined" != typeof self ? self : this).basicLightbox = e2();
        }
      })((function() {
        return (function e2(n2, t2, o2) {
          function r2(c3, u2) {
            if (!t2[c3]) {
              if (!n2[c3]) {
                var s2 = "function" == typeof __require && __require;
                if (!u2 && s2) return s2(c3, true);
                if (i3) return i3(c3, true);
                var a2 = new Error("Cannot find module '" + c3 + "'");
                throw a2.code = "MODULE_NOT_FOUND", a2;
              }
              var l2 = t2[c3] = { exports: {} };
              n2[c3][0].call(l2.exports, (function(e3) {
                return r2(n2[c3][1][e3] || e3);
              }), l2, l2.exports, e2, n2, t2, o2);
            }
            return t2[c3].exports;
          }
          for (var i3 = "function" == typeof __require && __require, c2 = 0; c2 < o2.length; c2++) r2(o2[c2]);
          return r2;
        })({ 1: [function(e2, n2, t2) {
          "use strict";
          Object.defineProperty(t2, "__esModule", { value: true }), t2.create = t2.visible = void 0;
          var o2 = function(e3) {
            var n3 = arguments.length > 1 && void 0 !== arguments[1] && arguments[1], t3 = document.createElement("div");
            return t3.innerHTML = e3.trim(), true === n3 ? t3.children : t3.firstChild;
          }, r2 = function(e3, n3) {
            var t3 = e3.children;
            return 1 === t3.length && t3[0].tagName === n3;
          }, i3 = function(e3) {
            return null != (e3 = e3 || document.querySelector(".basicLightbox")) && true === e3.ownerDocument.body.contains(e3);
          };
          t2.visible = i3;
          t2.create = function(e3, n3) {
            var t3 = (function(e4, n4) {
              var t4 = o2('\n		<div class="basicLightbox '.concat(n4.className, '">\n			<div class="basicLightbox__placeholder" role="dialog"></div>\n		</div>\n	')), i4 = t4.querySelector(".basicLightbox__placeholder");
              e4.forEach((function(e5) {
                return i4.appendChild(e5);
              }));
              var c3 = r2(i4, "IMG"), u3 = r2(i4, "VIDEO"), s2 = r2(i4, "IFRAME");
              return true === c3 && t4.classList.add("basicLightbox--img"), true === u3 && t4.classList.add("basicLightbox--video"), true === s2 && t4.classList.add("basicLightbox--iframe"), t4;
            })(e3 = (function(e4) {
              var n4 = "string" == typeof e4, t4 = e4 instanceof HTMLElement == 1;
              if (false === n4 && false === t4) throw new Error("Content must be a DOM element/node or string");
              return true === n4 ? Array.from(o2(e4, true)) : "TEMPLATE" === e4.tagName ? [e4.content.cloneNode(true)] : Array.from(e4.children);
            })(e3), n3 = (function() {
              var e4 = arguments.length > 0 && void 0 !== arguments[0] ? arguments[0] : {};
              if (null == (e4 = Object.assign({}, e4)).closable && (e4.closable = true), null == e4.className && (e4.className = ""), null == e4.onShow && (e4.onShow = function() {
              }), null == e4.onClose && (e4.onClose = function() {
              }), "boolean" != typeof e4.closable) throw new Error("Property `closable` must be a boolean");
              if ("string" != typeof e4.className) throw new Error("Property `className` must be a string");
              if ("function" != typeof e4.onShow) throw new Error("Property `onShow` must be a function");
              if ("function" != typeof e4.onClose) throw new Error("Property `onClose` must be a function");
              return e4;
            })(n3)), c2 = function(e4) {
              return false !== n3.onClose(u2) && (function(e5, n4) {
                return e5.classList.remove("basicLightbox--visible"), setTimeout((function() {
                  return false === i3(e5) || e5.parentElement.removeChild(e5), n4();
                }), 410), true;
              })(t3, (function() {
                if ("function" == typeof e4) return e4(u2);
              }));
            };
            true === n3.closable && t3.addEventListener("click", (function(e4) {
              e4.target === t3 && c2();
            }));
            var u2 = { element: function() {
              return t3;
            }, visible: function() {
              return i3(t3);
            }, show: function(e4) {
              return false !== n3.onShow(u2) && (function(e5, n4) {
                return document.body.appendChild(e5), setTimeout((function() {
                  requestAnimationFrame((function() {
                    return e5.classList.add("basicLightbox--visible"), n4();
                  }));
                }), 10), true;
              })(t3, (function() {
                if ("function" == typeof e4) return e4(u2);
              }));
            }, close: c2 };
            return u2;
          };
        }, {}] }, {}, [1])(1);
      }));
    }
  });

  // node_modules/quicklink/dist/quicklink.mjs
  function e(e2, r2) {
    (null == r2 || r2 > e2.length) && (r2 = e2.length);
    for (var n2 = 0, t2 = Array(r2); n2 < r2; n2++) t2[n2] = e2[n2];
    return t2;
  }
  function r(r2, n2) {
    var t2 = "undefined" != typeof Symbol && r2[Symbol.iterator] || r2["@@iterator"];
    if (t2) return (t2 = t2.call(r2)).next.bind(t2);
    if (Array.isArray(r2) || (t2 = (function(r3, n3) {
      if (r3) {
        if ("string" == typeof r3) return e(r3, n3);
        var t3 = {}.toString.call(r3).slice(8, -1);
        return "Object" === t3 && r3.constructor && (t3 = r3.constructor.name), "Map" === t3 || "Set" === t3 ? Array.from(r3) : "Arguments" === t3 || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(t3) ? e(r3, n3) : void 0;
      }
    })(r2)) || n2) {
      t2 && (r2 = t2);
      var o2 = 0;
      return function() {
        return o2 >= r2.length ? { done: true } : { done: false, value: r2[o2++] };
      };
    }
    throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
  }
  function n(e2, r2, n2, t2) {
    var o2 = { headers: { accept: "*/*" } };
    return r2 || (o2.mode = "no-cors"), n2 && (o2.credentials = "include"), o2.priority = t2 ? "high" : "low", window.fetch ? fetch(e2, o2) : (function(e3, r3) {
      return new Promise(function(n3, t3, o3) {
        (o3 = new XMLHttpRequest()).open("GET", e3, o3.withCredentials = r3), o3.setRequestHeader("Accept", "*/*"), o3.onload = function() {
          200 === o3.status ? n3() : t3();
        }, o3.send();
      });
    })(e2, n2);
  }
  var t;
  var o = (t = document.createElement("link")).relList && t.relList.supports && t.relList.supports("prefetch") ? function(e2, r2) {
    return new Promise(function(n2, t2, o2) {
      (o2 = document.createElement("link")).rel = "prefetch", o2.href = e2, r2 && o2.setAttribute("crossorigin", "anonymous"), o2.onload = n2, o2.onerror = t2, document.head.append(o2);
    });
  } : n;
  var i = window.requestIdleCallback || function(e2) {
    var r2 = Date.now();
    return setTimeout(function() {
      e2({ didTimeout: false, timeRemaining: function() {
        return Math.max(0, 50 - (Date.now() - r2));
      } });
    }, 1);
  };
  var a = /* @__PURE__ */ new Set();
  var c = /* @__PURE__ */ new Set();
  var s = false;
  function u(e2, r2) {
    return Array.isArray(r2) ? r2.some(function(r3) {
      return u(e2, r3);
    }) : (r2.test || r2).call(r2, e2.href, e2);
  }
  function l(e2) {
    return !e2 || (e2.saveData ? new Error("Save-Data is enabled") : !/2g/.test(e2.effectiveType) || new Error("network conditions are poor"));
  }
  function f(e2) {
    if (void 0 === e2 && (e2 = {}), !window.IntersectionObserver || !("isIntersecting" in IntersectionObserverEntry.prototype)) return function() {
    };
    var n2 = (function(e3) {
      e3 = e3 || 1;
      var r2 = [], n3 = 0;
      function t3() {
        n3 < e3 && r2.length > 0 && (r2.shift()(), n3++);
      }
      return [function(e4) {
        r2.push(e4) > 1 || t3();
      }, function() {
        n3--, t3();
      }];
    })(e2.throttle || 1 / 0), t2 = n2[0], o2 = n2[1], l2 = e2.limit || 1 / 0, f2 = e2.threshold || 0, h = e2.origins || [location.hostname], m = e2.ignores || [], p = e2.delay || 0, g = /* @__PURE__ */ new Map(), w = /* @__PURE__ */ new Map(), y = e2.timeoutFn || i, E = "function" == typeof e2.hrefFn && e2.hrefFn, b = e2.prerender || false;
    s = e2.prerenderAndPrefetch || false;
    var A = new IntersectionObserver(function(n3) {
      for (var i3, u2 = function() {
        var n4 = i3.value, u3 = g.get(n4.target.href) || /* @__PURE__ */ new Set();
        g.set(n4.target.href, u3), n4.isIntersecting ? (n4 = n4.target, u3.add(n4), (function(e3, r2) {
          r2 ? setTimeout(e3, r2) : e3();
        })(function() {
          u3 && u3.size && (b || s || A.unobserve(n4), (s || b) && c.size < l2 && !w.has(n4.href) ? v(E ? E(n4) : n4.href, e2.eagerness).then(function(e3) {
            for (var n5, t3 = r(e3); !(n5 = t3()).done; ) {
              var o3 = n5.value, i4 = o3[0], a2 = o3[1];
              w.set(i4, a2);
            }
          }).catch(function(r2) {
            if (!e2.onError) throw r2;
            e2.onError(r2);
          }) : a.size < l2 && !b && t2(function() {
            d(E ? E(n4) : n4.href, e2.priority, e2.checkAccessControlAllowOrigin, e2.checkAccessControlAllowCredentials, e2.onlyOnMouseover).then(o2).catch(function(r2) {
              o2(), e2.onError && e2.onError(r2);
            });
          }));
        }, p)) : (n4 = n4.target, u3.delete(n4), w.has(n4.href) && w.set((function(e3, r2) {
          var n5 = e3.get(r2);
          try {
            n5.remove(), e3.delete(r2);
          } catch (e4) {
            return e4;
          }
          return e3;
        })(w, n4.href)));
      }, f3 = r(n3); !(i3 = f3()).done; ) u2();
    }, { threshold: f2 });
    return y(function() {
      for (var n3, t3 = r(e2.el && e2.el.length > 0 && "A" === e2.el[0].nodeName ? e2.el : (e2.el || document).querySelectorAll("a")); !(n3 = t3()).done; ) {
        var o3 = n3.value;
        h.length && !h.includes(o3.hostname) || u(o3, m) || A.observe(o3);
      }
    }, { timeout: e2.timeout || 2e3 }), function() {
      a.clear(), A.disconnect();
    };
  }
  function d(e2, t2, i3, u2, f2) {
    var d2 = l(navigator.connection);
    return d2 instanceof Error ? Promise.reject(new Error("Cannot prefetch, " + d2.message)) : (c.size > 0 && !s && console.warn("[Warning] You are using both prefetching and prerendering on the same document"), Promise.all([e2].flat().map(function(e3) {
      return a.has(e3) ? [] : (a.add(e3), (function(e4, n2, t3) {
        for (var o2 = arguments.length, i4 = new Array(o2 > 3 ? o2 - 3 : 0), a2 = 3; a2 < o2; a2++) i4[a2 - 3] = arguments[a2];
        if (!t3) return e4.apply(void 0, [n2].concat(i4));
        for (var c2, s2 = document.querySelectorAll('a[href="' + n2 + '"]'), u3 = /* @__PURE__ */ new Map(), l2 = function() {
          var r2 = c2.value, t4 = function() {
            var a3 = setTimeout(function() {
              return r2.removeEventListener("mouseenter", t4), r2.removeEventListener("mouseleave", o3), e4.apply(void 0, [n2].concat(i4));
            }, 200);
            u3.set(r2, a3);
          }, o3 = function() {
            var e5 = u3.get(r2);
            e5 && (clearTimeout(e5), u3.delete(r2));
          };
          r2.addEventListener("mouseenter", t4), r2.addEventListener("mouseleave", o3);
        }, f3 = r(s2); !(c2 = f3()).done; ) l2();
      })(t2 ? n : o, new URL(e3, location.href).toString(), f2, i3, u2, t2));
    })));
  }
  function v(e2, n2) {
    void 0 === n2 && (n2 = "immediate"), e2 = [e2].flat();
    var t2 = l(navigator.connection);
    if (t2 instanceof Error) return Promise.reject(new Error("Cannot prerender, " + t2.message));
    if (!HTMLScriptElement.supports("speculationrules")) return d(e2, true, false, false, "moderate" === n2 || "conservative" === n2), Promise.reject(new Error("This browser does not support the speculation rules API. Falling back to prefetch."));
    for (var o2, i3 = r(e2); !(o2 = i3()).done; ) {
      var u2 = o2.value;
      c.add(u2);
    }
    a.size > 0 && !s && console.warn("[Warning] You are using both prefetching and prerendering on the same document");
    var f2 = (function(e3, n3) {
      var t3 = /* @__PURE__ */ new Map();
      try {
        for (var o3, i4 = r(e3); !(o3 = i4()).done; ) {
          var a2 = o3.value, c2 = document.createElement("script");
          c2.type = "speculationrules", c2.text = JSON.stringify({ prerender: [{ source: "list", urls: [a2], eagerness: n3 }] }), document.head.append(c2), t3.set(a2, c2);
        }
      } catch (e4) {
        return e4;
      }
      return t3;
    })(e2, n2);
    return f2.size > 0 ? Promise.resolve(f2) : Promise.reject(f2);
  }

  // node_modules/@thulite/core/assets/js/core.js
  var import_lazysizes = __toESM(require_lazysizes());
  var import_ls = __toESM(require_ls_native_loading());
  f({
    ignores: [
      /\/api\/?/,
      (uri) => uri.includes(".zip"),
      (uri, elem) => elem.hasAttribute("noprefetch"),
      (uri, elem) => elem.hash && elem.pathname === window.location.pathname
    ]
  });
  try {
    import_lazysizes.default.cfg.nativeLoading = {
      setLoadingAttribute: true,
      disableListeners: {
        scroll: true
      }
    };
    import_lazysizes.default.cfg.lazyClass = "lazyload";
    import_lazysizes.default.cfg.loadedClass = "lazyloaded";
    import_lazysizes.default.cfg.loadingClass = "lazyloading";
    import_lazysizes.default.cfg.expFactor = 1.5;
    if (typeof document !== "undefined") {
      document.documentElement.classList.add("lazysizes-enabled");
    }
  } catch (error) {
    if (typeof console !== "undefined" && console.warn) {
      console.warn("lazySizes initialization failed:", error);
    }
  }

  // ns-hugo-imp:C:\Users\TH282424\Rprojects\bego\node_modules\@thulite\doks-core\assets\js\clipboard.js
  var import_clipboard = __toESM(require_clipboard());
  (() => {
    "use strict";
    var cb = document.getElementsByClassName("highlight");
    for (var i3 = 0; i3 < cb.length; ++i3) {
      var element = cb[i3];
      element.insertAdjacentHTML(
        "afterbegin",
        '<div class="copy"><button title="Copy to clipboard" class="btn-copy" aria-label="Clipboard button"><div></div></button></div>'
      );
    }
    var clipboard = new import_clipboard.default(".btn-copy", {
      target: function(trigger) {
        return trigger.parentNode.nextElementSibling;
      }
    });
    clipboard.on("success", function(e2) {
      e2.clearSelection();
    });
    clipboard.on("error", function(e2) {
      console.error("Action:", e2.action);
      console.error("Trigger:", e2.trigger);
    });
  })();

  // ns-hugo-imp:C:\Users\TH282424\Rprojects\bego\node_modules\@thulite\doks-core\assets\js\to-top.js
  var topButton = document.getElementById("toTop");
  if (topButton !== null) {
    topButton.classList.remove("fade");
    window.onscroll = function() {
      scrollFunction();
    };
    topButton.addEventListener("click", topFunction);
  }
  function scrollFunction() {
    if (document.body.scrollTop > 270 || document.documentElement.scrollTop > 270) {
      topButton.classList.add("fade");
    } else {
      topButton.classList.remove("fade");
    }
  }
  function topFunction() {
    document.body.scrollTop = 0;
    document.documentElement.scrollTop = 0;
  }

  // ns-hugo-imp:C:\Users\TH282424\Rprojects\bego\node_modules\@thulite\doks-core\assets\js\tabs.js
  var i2;
  var allTabs = document.querySelectorAll("[data-toggle-tab]");
  var allPanes = document.querySelectorAll("[data-pane]");
  function toggleTabs(event) {
    if (event.target) {
      event.preventDefault();
      var clickedTab = event.currentTarget;
      var targetKey = clickedTab.getAttribute("data-toggle-tab");
    } else {
      var targetKey = event;
    }
    if (window.localStorage) {
      window.localStorage.setItem("configLangPref", targetKey);
    }
    var selectedTabs = document.querySelectorAll("[data-toggle-tab=" + targetKey + "]");
    var selectedPanes = document.querySelectorAll("[data-pane=" + targetKey + "]");
    for (var i3 = 0; i3 < allTabs.length; i3++) {
      allTabs[i3].classList.remove("active");
      allPanes[i3].classList.remove("active");
    }
    for (var i3 = 0; i3 < selectedTabs.length; i3++) {
      selectedTabs[i3].classList.add("active");
      selectedPanes[i3].classList.add("show", "active");
    }
  }
  for (i2 = 0; i2 < allTabs.length; i2++) {
    allTabs[i2].addEventListener("click", toggleTabs);
  }
  if (window.localStorage.getItem("configLangPref")) {
    toggleTabs(window.localStorage.getItem("configLangPref"));
  }

  // ns-hugo-imp:C:\Users\TH282424\Rprojects\bego\node_modules\@thulite\doks-core\assets\js\toc.js
  document.addEventListener("click", function(e2) {
    const tocMobile = e2.target.closest(".toc-mobile");
    if (tocMobile && e2.target.tagName === "A") {
      const details = tocMobile.querySelector("details");
      if (details && details.open) {
        details.open = false;
      }
    }
  });

  // ns-hugo-imp:C:\Users\TH282424\Rprojects\bego\node_modules\@thulite\doks-core\assets\js\copy-markdown.js
  var copyMarkdownBtn = document.getElementById("copy-markdown");
  if (copyMarkdownBtn) {
    copyMarkdownBtn.addEventListener("click", function(e2) {
      e2.preventDefault();
      const button = this;
      const currentPath = window.location.pathname;
      const markdownPath = currentPath.endsWith("/") ? currentPath + "index.md" : currentPath + "/index.md";
      button.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="mb-1 me-1"><path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M12 6l0 -3" /><path d="M16.25 7.75l2.15 -2.15" /><path d="M18 12l3 0" /><path d="M16.25 16.25l2.15 2.15" /><path d="M12 18l0 3" /><path d="M7.75 16.25l-2.15 2.15" /><path d="M6 12l-3 0" /><path d="M7.75 7.75l-2.15 -2.15" /></svg> Loading...';
      fetch(markdownPath).then((response) => {
        if (!response.ok) {
          throw new Error(`HTTP error! status: ${response.status}`);
        }
        return response.text();
      }).then((markdown) => {
        return navigator.clipboard.writeText(markdown);
      }).then(() => {
        button.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="mb-1 me-1"><path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M5 12l5 5l10 -10" /></svg> Copied!';
        setTimeout(() => {
          button.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="mb-1 me-1"><path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M7 9.667a2.667 2.667 0 0 1 2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 2.667v8.666a2.667 2.667 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 -2.667l0 -8.666" /><path d="M4.012 16.737a2.005 2.005 0 0 1 -1.012 -1.737v-10c0 -1.1 .9 -2 2 -2h10c.75 0 1.158 .385 1.5 1" /></svg> Copy Markdown';
        }, 2e3);
      }).catch((err) => {
        button.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="mb-1 me-1"><path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M18 6l-12 12" /><path d="M6 6l12 12" /></svg> Error';
        console.error("Failed to copy markdown:", err);
        setTimeout(() => {
          button.innerHTML = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="mb-1 me-1"><path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M7 9.667a2.667 2.667 0 0 1 2.667 -2.667h8.666a2.667 2.667 0 0 1 2.667 2.667v8.666a2.667 2.667 0 0 1 -2.667 2.667h-8.666a2.667 2.667 0 0 1 -2.667 -2.667l0 -8.666" /><path d="M4.012 16.737a2.005 2.005 0 0 1 -1.012 -1.737v-10c0 -1.1 .9 -2 2 -2h10c.75 0 1.158 .385 1.5 1" /></svg> Copy Markdown';
        }, 2e3);
      });
    });
  }

  // ns-hugo-imp:C:\Users\TH282424\Rprojects\bego\node_modules\@thulite\doks-core\assets\js\basiclightbox.js
  var basicLightbox = __toESM(require_basicLightbox_min());
  var images = document.querySelectorAll(".lightbox img, img.lightbox");
  images.forEach((img) => {
    const html = img.outerHTML;
    const handleEscapeKey = (event) => {
      if (event.key === "Escape") {
        instance.close();
      }
    };
    const instance = basicLightbox.create(html, {
      onShow: () => {
        document.addEventListener("keydown", handleEscapeKey);
      },
      onClose: () => {
        document.removeEventListener("keydown", handleEscapeKey);
      }
    });
    img.onclick = () => {
      instance.show();
    };
  });
})();
/*! Bundled license information:

clipboard/dist/clipboard.js:
  (*!
   * clipboard.js v2.0.11
   * https://clipboardjs.com/
   *
   * Licensed MIT © Zeno Rocha
   *)

@thulite/doks-core/assets/js/clipboard.js:
  (*!
   * clipboard.js for Bootstrap based Thulite sites
   * Copyright 2021-2024 Thulite
   * Licensed under the MIT License
   *)
*/
//# sourceMappingURL=data:application/json;base64,ewogICJ2ZXJzaW9uIjogMywKICAic291cmNlcyI6IFsiLi4vbm9kZV9tb2R1bGVzL2xhenlzaXplcy9sYXp5c2l6ZXMuanMiLCAiLi4vbm9kZV9tb2R1bGVzL2xhenlzaXplcy9wbHVnaW5zL25hdGl2ZS1sb2FkaW5nL2xzLm5hdGl2ZS1sb2FkaW5nLmpzIiwgIi4uL25vZGVfbW9kdWxlcy9jbGlwYm9hcmQvZGlzdC9jbGlwYm9hcmQuanMiLCAiLi4vbm9kZV9tb2R1bGVzL2Jhc2ljbGlnaHRib3gvZGlzdC9iYXNpY0xpZ2h0Ym94Lm1pbi5qcyIsICIuLi9ub2RlX21vZHVsZXMvcXVpY2tsaW5rL2Rpc3QvcXVpY2tsaW5rLm1qcyIsICIuLi9ub2RlX21vZHVsZXMvQHRodWxpdGUvY29yZS9hc3NldHMvanMvY29yZS5qcyIsICJucy1odWdvLWltcDpDOlxcVXNlcnNcXFRIMjgyNDI0XFxScHJvamVjdHNcXGJlZ29cXG5vZGVfbW9kdWxlc1xcQHRodWxpdGVcXGRva3MtY29yZVxcYXNzZXRzXFxqc1xcY2xpcGJvYXJkLmpzIiwgIm5zLWh1Z28taW1wOkM6XFxVc2Vyc1xcVEgyODI0MjRcXFJwcm9qZWN0c1xcYmVnb1xcbm9kZV9tb2R1bGVzXFxAdGh1bGl0ZVxcZG9rcy1jb3JlXFxhc3NldHNcXGpzXFx0by10b3AuanMiLCAibnMtaHVnby1pbXA6QzpcXFVzZXJzXFxUSDI4MjQyNFxcUnByb2plY3RzXFxiZWdvXFxub2RlX21vZHVsZXNcXEB0aHVsaXRlXFxkb2tzLWNvcmVcXGFzc2V0c1xcanNcXHRhYnMuanMiLCAibnMtaHVnby1pbXA6QzpcXFVzZXJzXFxUSDI4MjQyNFxcUnByb2plY3RzXFxiZWdvXFxub2RlX21vZHVsZXNcXEB0aHVsaXRlXFxkb2tzLWNvcmVcXGFzc2V0c1xcanNcXHRvYy5qcyIsICJucy1odWdvLWltcDpDOlxcVXNlcnNcXFRIMjgyNDI0XFxScHJvamVjdHNcXGJlZ29cXG5vZGVfbW9kdWxlc1xcQHRodWxpdGVcXGRva3MtY29yZVxcYXNzZXRzXFxqc1xcY29weS1tYXJrZG93bi5qcyIsICJucy1odWdvLWltcDpDOlxcVXNlcnNcXFRIMjgyNDI0XFxScHJvamVjdHNcXGJlZ29cXG5vZGVfbW9kdWxlc1xcQHRodWxpdGVcXGRva3MtY29yZVxcYXNzZXRzXFxqc1xcYmFzaWNsaWdodGJveC5qcyJdLAogICJzb3VyY2VzQ29udGVudCI6IFsiKGZ1bmN0aW9uKHdpbmRvdywgZmFjdG9yeSkge1xuXHR2YXIgbGF6eVNpemVzID0gZmFjdG9yeSh3aW5kb3csIHdpbmRvdy5kb2N1bWVudCwgRGF0ZSk7XG5cdHdpbmRvdy5sYXp5U2l6ZXMgPSBsYXp5U2l6ZXM7XG5cdGlmKHR5cGVvZiBtb2R1bGUgPT0gJ29iamVjdCcgJiYgbW9kdWxlLmV4cG9ydHMpe1xuXHRcdG1vZHVsZS5leHBvcnRzID0gbGF6eVNpemVzO1xuXHR9XG59KHR5cGVvZiB3aW5kb3cgIT0gJ3VuZGVmaW5lZCcgP1xuICAgICAgd2luZG93IDoge30sIFxuLyoqXG4gKiBpbXBvcnQoXCIuL3R5cGVzL2dsb2JhbFwiKVxuICogQHR5cGVkZWYgeyBpbXBvcnQoXCIuL3R5cGVzL2xhenlzaXplcy1jb25maWdcIikuTGF6eVNpemVzQ29uZmlnUGFydGlhbCB9IExhenlTaXplc0NvbmZpZ1BhcnRpYWxcbiAqL1xuZnVuY3Rpb24gbCh3aW5kb3csIGRvY3VtZW50LCBEYXRlKSB7IC8vIFBhc3MgaW4gdGhlIHdpbmRvdyBEYXRlIGZ1bmN0aW9uIGFsc28gZm9yIFNTUiBiZWNhdXNlIHRoZSBEYXRlIGNsYXNzIGNhbiBiZSBsb3N0XG5cdCd1c2Ugc3RyaWN0Jztcblx0Lypqc2hpbnQgZXFudWxsOnRydWUgKi9cblxuXHR2YXIgbGF6eXNpemVzLFxuXHRcdC8qKlxuXHRcdCAqIEB0eXBlIHsgTGF6eVNpemVzQ29uZmlnUGFydGlhbCB9XG5cdFx0ICovXG5cdFx0bGF6eVNpemVzQ2ZnO1xuXG5cdChmdW5jdGlvbigpe1xuXHRcdHZhciBwcm9wO1xuXG5cdFx0dmFyIGxhenlTaXplc0RlZmF1bHRzID0ge1xuXHRcdFx0bGF6eUNsYXNzOiAnbGF6eWxvYWQnLFxuXHRcdFx0bG9hZGVkQ2xhc3M6ICdsYXp5bG9hZGVkJyxcblx0XHRcdGxvYWRpbmdDbGFzczogJ2xhenlsb2FkaW5nJyxcblx0XHRcdHByZWxvYWRDbGFzczogJ2xhenlwcmVsb2FkJyxcblx0XHRcdGVycm9yQ2xhc3M6ICdsYXp5ZXJyb3InLFxuXHRcdFx0Ly9zdHJpY3RDbGFzczogJ2xhenlzdHJpY3QnLFxuXHRcdFx0YXV0b3NpemVzQ2xhc3M6ICdsYXp5YXV0b3NpemVzJyxcblx0XHRcdGZhc3RMb2FkZWRDbGFzczogJ2xzLWlzLWNhY2hlZCcsXG5cdFx0XHRpZnJhbWVMb2FkTW9kZTogMCxcblx0XHRcdHNyY0F0dHI6ICdkYXRhLXNyYycsXG5cdFx0XHRzcmNzZXRBdHRyOiAnZGF0YS1zcmNzZXQnLFxuXHRcdFx0c2l6ZXNBdHRyOiAnZGF0YS1zaXplcycsXG5cdFx0XHQvL3ByZWxvYWRBZnRlckxvYWQ6IGZhbHNlLFxuXHRcdFx0bWluU2l6ZTogNDAsXG5cdFx0XHRjdXN0b21NZWRpYToge30sXG5cdFx0XHRpbml0OiB0cnVlLFxuXHRcdFx0ZXhwRmFjdG9yOiAxLjUsXG5cdFx0XHRoRmFjOiAwLjgsXG5cdFx0XHRsb2FkTW9kZTogMixcblx0XHRcdGxvYWRIaWRkZW46IHRydWUsXG5cdFx0XHRyaWNUaW1lb3V0OiAwLFxuXHRcdFx0dGhyb3R0bGVEZWxheTogMTI1LFxuXHRcdH07XG5cblx0XHRsYXp5U2l6ZXNDZmcgPSB3aW5kb3cubGF6eVNpemVzQ29uZmlnIHx8IHdpbmRvdy5sYXp5c2l6ZXNDb25maWcgfHwge307XG5cblx0XHRmb3IocHJvcCBpbiBsYXp5U2l6ZXNEZWZhdWx0cyl7XG5cdFx0XHRpZighKHByb3AgaW4gbGF6eVNpemVzQ2ZnKSl7XG5cdFx0XHRcdGxhenlTaXplc0NmZ1twcm9wXSA9IGxhenlTaXplc0RlZmF1bHRzW3Byb3BdO1xuXHRcdFx0fVxuXHRcdH1cblx0fSkoKTtcblxuXHRpZiAoIWRvY3VtZW50IHx8ICFkb2N1bWVudC5nZXRFbGVtZW50c0J5Q2xhc3NOYW1lKSB7XG5cdFx0cmV0dXJuIHtcblx0XHRcdGluaXQ6IGZ1bmN0aW9uICgpIHt9LFxuXHRcdFx0LyoqXG5cdFx0XHQgKiBAdHlwZSB7IExhenlTaXplc0NvbmZpZ1BhcnRpYWwgfVxuXHRcdFx0ICovXG5cdFx0XHRjZmc6IGxhenlTaXplc0NmZyxcblx0XHRcdC8qKlxuXHRcdFx0ICogQHR5cGUgeyB0cnVlIH1cblx0XHRcdCAqL1xuXHRcdFx0bm9TdXBwb3J0OiB0cnVlLFxuXHRcdH07XG5cdH1cblxuXHR2YXIgZG9jRWxlbSA9IGRvY3VtZW50LmRvY3VtZW50RWxlbWVudDtcblxuXHR2YXIgc3VwcG9ydFBpY3R1cmUgPSB3aW5kb3cuSFRNTFBpY3R1cmVFbGVtZW50O1xuXG5cdHZhciBfYWRkRXZlbnRMaXN0ZW5lciA9ICdhZGRFdmVudExpc3RlbmVyJztcblxuXHR2YXIgX2dldEF0dHJpYnV0ZSA9ICdnZXRBdHRyaWJ1dGUnO1xuXG5cdC8qKlxuXHQgKiBVcGRhdGUgdG8gYmluZCB0byB3aW5kb3cgYmVjYXVzZSAndGhpcycgYmVjb21lcyBudWxsIGR1cmluZyBTU1Jcblx0ICogYnVpbGRzLlxuXHQgKi9cblx0dmFyIGFkZEV2ZW50TGlzdGVuZXIgPSB3aW5kb3dbX2FkZEV2ZW50TGlzdGVuZXJdLmJpbmQod2luZG93KTtcblxuXHR2YXIgc2V0VGltZW91dCA9IHdpbmRvdy5zZXRUaW1lb3V0O1xuXG5cdHZhciByZXF1ZXN0QW5pbWF0aW9uRnJhbWUgPSB3aW5kb3cucmVxdWVzdEFuaW1hdGlvbkZyYW1lIHx8IHNldFRpbWVvdXQ7XG5cblx0dmFyIHJlcXVlc3RJZGxlQ2FsbGJhY2sgPSB3aW5kb3cucmVxdWVzdElkbGVDYWxsYmFjaztcblxuXHR2YXIgcmVnUGljdHVyZSA9IC9ecGljdHVyZSQvaTtcblxuXHR2YXIgbG9hZEV2ZW50cyA9IFsnbG9hZCcsICdlcnJvcicsICdsYXp5aW5jbHVkZWQnLCAnX2xhenlsb2FkZWQnXTtcblxuXHR2YXIgcmVnQ2xhc3NDYWNoZSA9IHt9O1xuXG5cdHZhciBmb3JFYWNoID0gQXJyYXkucHJvdG90eXBlLmZvckVhY2g7XG5cblx0LyoqXG5cdCAqIEBwYXJhbSBlbGUge0VsZW1lbnR9XG5cdCAqIEBwYXJhbSBjbHMge3N0cmluZ31cblx0ICovXG5cdHZhciBoYXNDbGFzcyA9IGZ1bmN0aW9uKGVsZSwgY2xzKSB7XG5cdFx0aWYoIXJlZ0NsYXNzQ2FjaGVbY2xzXSl7XG5cdFx0XHRyZWdDbGFzc0NhY2hlW2Nsc10gPSBuZXcgUmVnRXhwKCcoXFxcXHN8XiknK2NscysnKFxcXFxzfCQpJyk7XG5cdFx0fVxuXHRcdHJldHVybiByZWdDbGFzc0NhY2hlW2Nsc10udGVzdChlbGVbX2dldEF0dHJpYnV0ZV0oJ2NsYXNzJykgfHwgJycpICYmIHJlZ0NsYXNzQ2FjaGVbY2xzXTtcblx0fTtcblxuXHQvKipcblx0ICogQHBhcmFtIGVsZSB7RWxlbWVudH1cblx0ICogQHBhcmFtIGNscyB7c3RyaW5nfVxuXHQgKi9cblx0dmFyIGFkZENsYXNzID0gZnVuY3Rpb24oZWxlLCBjbHMpIHtcblx0XHRpZiAoIWhhc0NsYXNzKGVsZSwgY2xzKSl7XG5cdFx0XHRlbGUuc2V0QXR0cmlidXRlKCdjbGFzcycsIChlbGVbX2dldEF0dHJpYnV0ZV0oJ2NsYXNzJykgfHwgJycpLnRyaW0oKSArICcgJyArIGNscyk7XG5cdFx0fVxuXHR9O1xuXG5cdC8qKlxuXHQgKiBAcGFyYW0gZWxlIHtFbGVtZW50fVxuXHQgKiBAcGFyYW0gY2xzIHtzdHJpbmd9XG5cdCAqL1xuXHR2YXIgcmVtb3ZlQ2xhc3MgPSBmdW5jdGlvbihlbGUsIGNscykge1xuXHRcdHZhciByZWc7XG5cdFx0aWYgKChyZWcgPSBoYXNDbGFzcyhlbGUsY2xzKSkpIHtcblx0XHRcdGVsZS5zZXRBdHRyaWJ1dGUoJ2NsYXNzJywgKGVsZVtfZ2V0QXR0cmlidXRlXSgnY2xhc3MnKSB8fCAnJykucmVwbGFjZShyZWcsICcgJykpO1xuXHRcdH1cblx0fTtcblxuXHR2YXIgYWRkUmVtb3ZlTG9hZEV2ZW50cyA9IGZ1bmN0aW9uKGRvbSwgZm4sIGFkZCl7XG5cdFx0dmFyIGFjdGlvbiA9IGFkZCA/IF9hZGRFdmVudExpc3RlbmVyIDogJ3JlbW92ZUV2ZW50TGlzdGVuZXInO1xuXHRcdGlmKGFkZCl7XG5cdFx0XHRhZGRSZW1vdmVMb2FkRXZlbnRzKGRvbSwgZm4pO1xuXHRcdH1cblx0XHRsb2FkRXZlbnRzLmZvckVhY2goZnVuY3Rpb24oZXZ0KXtcblx0XHRcdGRvbVthY3Rpb25dKGV2dCwgZm4pO1xuXHRcdH0pO1xuXHR9O1xuXG5cdC8qKlxuXHQgKiBAcGFyYW0gZWxlbSB7IEVsZW1lbnQgfVxuXHQgKiBAcGFyYW0gbmFtZSB7IHN0cmluZyB9XG5cdCAqIEBwYXJhbSBkZXRhaWwgeyBhbnkgfVxuXHQgKiBAcGFyYW0gbm9CdWJibGVzIHsgYm9vbGVhbiB9XG5cdCAqIEBwYXJhbSBub0NhbmNlbGFibGUgeyBib29sZWFuIH1cblx0ICogQHJldHVybnMgeyBDdXN0b21FdmVudCB9XG5cdCAqL1xuXHR2YXIgdHJpZ2dlckV2ZW50ID0gZnVuY3Rpb24oZWxlbSwgbmFtZSwgZGV0YWlsLCBub0J1YmJsZXMsIG5vQ2FuY2VsYWJsZSl7XG5cdFx0dmFyIGV2ZW50ID0gZG9jdW1lbnQuY3JlYXRlRXZlbnQoJ0V2ZW50Jyk7XG5cblx0XHRpZighZGV0YWlsKXtcblx0XHRcdGRldGFpbCA9IHt9O1xuXHRcdH1cblxuXHRcdGRldGFpbC5pbnN0YW5jZSA9IGxhenlzaXplcztcblxuXHRcdGV2ZW50LmluaXRFdmVudChuYW1lLCAhbm9CdWJibGVzLCAhbm9DYW5jZWxhYmxlKTtcblxuXHRcdGV2ZW50LmRldGFpbCA9IGRldGFpbDtcblxuXHRcdGVsZW0uZGlzcGF0Y2hFdmVudChldmVudCk7XG5cdFx0cmV0dXJuIGV2ZW50O1xuXHR9O1xuXG5cdHZhciB1cGRhdGVQb2x5ZmlsbCA9IGZ1bmN0aW9uIChlbCwgZnVsbCl7XG5cdFx0dmFyIHBvbHlmaWxsO1xuXHRcdGlmKCAhc3VwcG9ydFBpY3R1cmUgJiYgKCBwb2x5ZmlsbCA9ICh3aW5kb3cucGljdHVyZWZpbGwgfHwgbGF6eVNpemVzQ2ZnLnBmKSApICl7XG5cdFx0XHRpZihmdWxsICYmIGZ1bGwuc3JjICYmICFlbFtfZ2V0QXR0cmlidXRlXSgnc3Jjc2V0Jykpe1xuXHRcdFx0XHRlbC5zZXRBdHRyaWJ1dGUoJ3NyY3NldCcsIGZ1bGwuc3JjKTtcblx0XHRcdH1cblx0XHRcdHBvbHlmaWxsKHtyZWV2YWx1YXRlOiB0cnVlLCBlbGVtZW50czogW2VsXX0pO1xuXHRcdH0gZWxzZSBpZihmdWxsICYmIGZ1bGwuc3JjKXtcblx0XHRcdGVsLnNyYyA9IGZ1bGwuc3JjO1xuXHRcdH1cblx0fTtcblxuXHR2YXIgZ2V0Q1NTID0gZnVuY3Rpb24gKGVsZW0sIHN0eWxlKXtcblx0XHRyZXR1cm4gKGdldENvbXB1dGVkU3R5bGUoZWxlbSwgbnVsbCkgfHwge30pW3N0eWxlXTtcblx0fTtcblxuXHQvKipcblx0ICpcblx0ICogQHBhcmFtIGVsZW0geyBFbGVtZW50IH1cblx0ICogQHBhcmFtIHBhcmVudCB7IEVsZW1lbnQgfVxuXHQgKiBAcGFyYW0gW3dpZHRoXSB7bnVtYmVyfVxuXHQgKiBAcmV0dXJucyB7bnVtYmVyfVxuXHQgKi9cblx0dmFyIGdldFdpZHRoID0gZnVuY3Rpb24oZWxlbSwgcGFyZW50LCB3aWR0aCl7XG5cdFx0d2lkdGggPSB3aWR0aCB8fCBlbGVtLm9mZnNldFdpZHRoO1xuXG5cdFx0d2hpbGUod2lkdGggPCBsYXp5U2l6ZXNDZmcubWluU2l6ZSAmJiBwYXJlbnQgJiYgIWVsZW0uX2xhenlzaXplc1dpZHRoKXtcblx0XHRcdHdpZHRoID0gIHBhcmVudC5vZmZzZXRXaWR0aDtcblx0XHRcdHBhcmVudCA9IHBhcmVudC5wYXJlbnROb2RlO1xuXHRcdH1cblxuXHRcdHJldHVybiB3aWR0aDtcblx0fTtcblxuXHR2YXIgckFGID0gKGZ1bmN0aW9uKCl7XG5cdFx0dmFyIHJ1bm5pbmcsIHdhaXRpbmc7XG5cdFx0dmFyIGZpcnN0Rm5zID0gW107XG5cdFx0dmFyIHNlY29uZEZucyA9IFtdO1xuXHRcdHZhciBmbnMgPSBmaXJzdEZucztcblxuXHRcdHZhciBydW4gPSBmdW5jdGlvbigpe1xuXHRcdFx0dmFyIHJ1bkZucyA9IGZucztcblxuXHRcdFx0Zm5zID0gZmlyc3RGbnMubGVuZ3RoID8gc2Vjb25kRm5zIDogZmlyc3RGbnM7XG5cblx0XHRcdHJ1bm5pbmcgPSB0cnVlO1xuXHRcdFx0d2FpdGluZyA9IGZhbHNlO1xuXG5cdFx0XHR3aGlsZShydW5GbnMubGVuZ3RoKXtcblx0XHRcdFx0cnVuRm5zLnNoaWZ0KCkoKTtcblx0XHRcdH1cblxuXHRcdFx0cnVubmluZyA9IGZhbHNlO1xuXHRcdH07XG5cblx0XHR2YXIgcmFmQmF0Y2ggPSBmdW5jdGlvbihmbiwgcXVldWUpe1xuXHRcdFx0aWYocnVubmluZyAmJiAhcXVldWUpe1xuXHRcdFx0XHRmbi5hcHBseSh0aGlzLCBhcmd1bWVudHMpO1xuXHRcdFx0fSBlbHNlIHtcblx0XHRcdFx0Zm5zLnB1c2goZm4pO1xuXG5cdFx0XHRcdGlmKCF3YWl0aW5nKXtcblx0XHRcdFx0XHR3YWl0aW5nID0gdHJ1ZTtcblx0XHRcdFx0XHQoZG9jdW1lbnQuaGlkZGVuID8gc2V0VGltZW91dCA6IHJlcXVlc3RBbmltYXRpb25GcmFtZSkocnVuKTtcblx0XHRcdFx0fVxuXHRcdFx0fVxuXHRcdH07XG5cblx0XHRyYWZCYXRjaC5fbHNGbHVzaCA9IHJ1bjtcblxuXHRcdHJldHVybiByYWZCYXRjaDtcblx0fSkoKTtcblxuXHR2YXIgckFGSXQgPSBmdW5jdGlvbihmbiwgc2ltcGxlKXtcblx0XHRyZXR1cm4gc2ltcGxlID9cblx0XHRcdGZ1bmN0aW9uKCkge1xuXHRcdFx0XHRyQUYoZm4pO1xuXHRcdFx0fSA6XG5cdFx0XHRmdW5jdGlvbigpe1xuXHRcdFx0XHR2YXIgdGhhdCA9IHRoaXM7XG5cdFx0XHRcdHZhciBhcmdzID0gYXJndW1lbnRzO1xuXHRcdFx0XHRyQUYoZnVuY3Rpb24oKXtcblx0XHRcdFx0XHRmbi5hcHBseSh0aGF0LCBhcmdzKTtcblx0XHRcdFx0fSk7XG5cdFx0XHR9XG5cdFx0O1xuXHR9O1xuXG5cdHZhciB0aHJvdHRsZSA9IGZ1bmN0aW9uKGZuKXtcblx0XHR2YXIgcnVubmluZztcblx0XHR2YXIgbGFzdFRpbWUgPSAwO1xuXHRcdHZhciBnRGVsYXkgPSBsYXp5U2l6ZXNDZmcudGhyb3R0bGVEZWxheTtcblx0XHR2YXIgcklDVGltZW91dCA9IGxhenlTaXplc0NmZy5yaWNUaW1lb3V0O1xuXHRcdHZhciBydW4gPSBmdW5jdGlvbigpe1xuXHRcdFx0cnVubmluZyA9IGZhbHNlO1xuXHRcdFx0bGFzdFRpbWUgPSBEYXRlLm5vdygpO1xuXHRcdFx0Zm4oKTtcblx0XHR9O1xuXHRcdHZhciBpZGxlQ2FsbGJhY2sgPSByZXF1ZXN0SWRsZUNhbGxiYWNrICYmIHJJQ1RpbWVvdXQgPiA0OSA/XG5cdFx0XHRmdW5jdGlvbigpe1xuXHRcdFx0XHRyZXF1ZXN0SWRsZUNhbGxiYWNrKHJ1biwge3RpbWVvdXQ6IHJJQ1RpbWVvdXR9KTtcblxuXHRcdFx0XHRpZihySUNUaW1lb3V0ICE9PSBsYXp5U2l6ZXNDZmcucmljVGltZW91dCl7XG5cdFx0XHRcdFx0cklDVGltZW91dCA9IGxhenlTaXplc0NmZy5yaWNUaW1lb3V0O1xuXHRcdFx0XHR9XG5cdFx0XHR9IDpcblx0XHRcdHJBRkl0KGZ1bmN0aW9uKCl7XG5cdFx0XHRcdHNldFRpbWVvdXQocnVuKTtcblx0XHRcdH0sIHRydWUpXG5cdFx0O1xuXG5cdFx0cmV0dXJuIGZ1bmN0aW9uKGlzUHJpb3JpdHkpe1xuXHRcdFx0dmFyIGRlbGF5O1xuXG5cdFx0XHRpZigoaXNQcmlvcml0eSA9IGlzUHJpb3JpdHkgPT09IHRydWUpKXtcblx0XHRcdFx0cklDVGltZW91dCA9IDMzO1xuXHRcdFx0fVxuXG5cdFx0XHRpZihydW5uaW5nKXtcblx0XHRcdFx0cmV0dXJuO1xuXHRcdFx0fVxuXG5cdFx0XHRydW5uaW5nID0gIHRydWU7XG5cblx0XHRcdGRlbGF5ID0gZ0RlbGF5IC0gKERhdGUubm93KCkgLSBsYXN0VGltZSk7XG5cblx0XHRcdGlmKGRlbGF5IDwgMCl7XG5cdFx0XHRcdGRlbGF5ID0gMDtcblx0XHRcdH1cblxuXHRcdFx0aWYoaXNQcmlvcml0eSB8fCBkZWxheSA8IDkpe1xuXHRcdFx0XHRpZGxlQ2FsbGJhY2soKTtcblx0XHRcdH0gZWxzZSB7XG5cdFx0XHRcdHNldFRpbWVvdXQoaWRsZUNhbGxiYWNrLCBkZWxheSk7XG5cdFx0XHR9XG5cdFx0fTtcblx0fTtcblxuXHQvL2Jhc2VkIG9uIGh0dHA6Ly9tb2Rlcm5qYXZhc2NyaXB0LmJsb2dzcG90LmRlLzIwMTMvMDgvYnVpbGRpbmctYmV0dGVyLWRlYm91bmNlLmh0bWxcblx0dmFyIGRlYm91bmNlID0gZnVuY3Rpb24oZnVuYykge1xuXHRcdHZhciB0aW1lb3V0LCB0aW1lc3RhbXA7XG5cdFx0dmFyIHdhaXQgPSA5OTtcblx0XHR2YXIgcnVuID0gZnVuY3Rpb24oKXtcblx0XHRcdHRpbWVvdXQgPSBudWxsO1xuXHRcdFx0ZnVuYygpO1xuXHRcdH07XG5cdFx0dmFyIGxhdGVyID0gZnVuY3Rpb24oKSB7XG5cdFx0XHR2YXIgbGFzdCA9IERhdGUubm93KCkgLSB0aW1lc3RhbXA7XG5cblx0XHRcdGlmIChsYXN0IDwgd2FpdCkge1xuXHRcdFx0XHRzZXRUaW1lb3V0KGxhdGVyLCB3YWl0IC0gbGFzdCk7XG5cdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHQocmVxdWVzdElkbGVDYWxsYmFjayB8fCBydW4pKHJ1bik7XG5cdFx0XHR9XG5cdFx0fTtcblxuXHRcdHJldHVybiBmdW5jdGlvbigpIHtcblx0XHRcdHRpbWVzdGFtcCA9IERhdGUubm93KCk7XG5cblx0XHRcdGlmICghdGltZW91dCkge1xuXHRcdFx0XHR0aW1lb3V0ID0gc2V0VGltZW91dChsYXRlciwgd2FpdCk7XG5cdFx0XHR9XG5cdFx0fTtcblx0fTtcblxuXHR2YXIgbG9hZGVyID0gKGZ1bmN0aW9uKCl7XG5cdFx0dmFyIHByZWxvYWRFbGVtcywgaXNDb21wbGV0ZWQsIHJlc2V0UHJlbG9hZGluZ1RpbWVyLCBsb2FkTW9kZSwgc3RhcnRlZDtcblxuXHRcdHZhciBlTHZXLCBlbHZILCBlTHRvcCwgZUxsZWZ0LCBlTHJpZ2h0LCBlTGJvdHRvbSwgaXNCb2R5SGlkZGVuO1xuXG5cdFx0dmFyIHJlZ0ltZyA9IC9eaW1nJC9pO1xuXHRcdHZhciByZWdJZnJhbWUgPSAvXmlmcmFtZSQvaTtcblxuXHRcdHZhciBzdXBwb3J0U2Nyb2xsID0gKCdvbnNjcm9sbCcgaW4gd2luZG93KSAmJiAhKC8oZ2xlfGluZylib3QvLnRlc3QobmF2aWdhdG9yLnVzZXJBZ2VudCkpO1xuXG5cdFx0dmFyIHNocmlua0V4cGFuZCA9IDA7XG5cdFx0dmFyIGN1cnJlbnRFeHBhbmQgPSAwO1xuXG5cdFx0dmFyIGlzTG9hZGluZyA9IDA7XG5cdFx0dmFyIGxvd1J1bnMgPSAtMTtcblxuXHRcdHZhciByZXNldFByZWxvYWRpbmcgPSBmdW5jdGlvbihlKXtcblx0XHRcdGlzTG9hZGluZy0tO1xuXHRcdFx0aWYoIWUgfHwgaXNMb2FkaW5nIDwgMCB8fCAhZS50YXJnZXQpe1xuXHRcdFx0XHRpc0xvYWRpbmcgPSAwO1xuXHRcdFx0fVxuXHRcdH07XG5cblx0XHR2YXIgaXNWaXNpYmxlID0gZnVuY3Rpb24gKGVsZW0pIHtcblx0XHRcdGlmIChpc0JvZHlIaWRkZW4gPT0gbnVsbCkge1xuXHRcdFx0XHRpc0JvZHlIaWRkZW4gPSBnZXRDU1MoZG9jdW1lbnQuYm9keSwgJ3Zpc2liaWxpdHknKSA9PSAnaGlkZGVuJztcblx0XHRcdH1cblxuXHRcdFx0cmV0dXJuIGlzQm9keUhpZGRlbiB8fCAhKGdldENTUyhlbGVtLnBhcmVudE5vZGUsICd2aXNpYmlsaXR5JykgPT0gJ2hpZGRlbicgJiYgZ2V0Q1NTKGVsZW0sICd2aXNpYmlsaXR5JykgPT0gJ2hpZGRlbicpO1xuXHRcdH07XG5cblx0XHR2YXIgaXNOZXN0ZWRWaXNpYmxlID0gZnVuY3Rpb24oZWxlbSwgZWxlbUV4cGFuZCl7XG5cdFx0XHR2YXIgb3V0ZXJSZWN0O1xuXHRcdFx0dmFyIHBhcmVudCA9IGVsZW07XG5cdFx0XHR2YXIgdmlzaWJsZSA9IGlzVmlzaWJsZShlbGVtKTtcblxuXHRcdFx0ZUx0b3AgLT0gZWxlbUV4cGFuZDtcblx0XHRcdGVMYm90dG9tICs9IGVsZW1FeHBhbmQ7XG5cdFx0XHRlTGxlZnQgLT0gZWxlbUV4cGFuZDtcblx0XHRcdGVMcmlnaHQgKz0gZWxlbUV4cGFuZDtcblxuXHRcdFx0d2hpbGUodmlzaWJsZSAmJiAocGFyZW50ID0gcGFyZW50Lm9mZnNldFBhcmVudCkgJiYgcGFyZW50ICE9IGRvY3VtZW50LmJvZHkgJiYgcGFyZW50ICE9IGRvY0VsZW0pe1xuXHRcdFx0XHR2aXNpYmxlID0gKChnZXRDU1MocGFyZW50LCAnb3BhY2l0eScpIHx8IDEpID4gMCk7XG5cblx0XHRcdFx0aWYodmlzaWJsZSAmJiBnZXRDU1MocGFyZW50LCAnb3ZlcmZsb3cnKSAhPSAndmlzaWJsZScpe1xuXHRcdFx0XHRcdG91dGVyUmVjdCA9IHBhcmVudC5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKTtcblx0XHRcdFx0XHR2aXNpYmxlID0gZUxyaWdodCA+IG91dGVyUmVjdC5sZWZ0ICYmXG5cdFx0XHRcdFx0XHRlTGxlZnQgPCBvdXRlclJlY3QucmlnaHQgJiZcblx0XHRcdFx0XHRcdGVMYm90dG9tID4gb3V0ZXJSZWN0LnRvcCAtIDEgJiZcblx0XHRcdFx0XHRcdGVMdG9wIDwgb3V0ZXJSZWN0LmJvdHRvbSArIDFcblx0XHRcdFx0XHQ7XG5cdFx0XHRcdH1cblx0XHRcdH1cblxuXHRcdFx0cmV0dXJuIHZpc2libGU7XG5cdFx0fTtcblxuXHRcdHZhciBjaGVja0VsZW1lbnRzID0gZnVuY3Rpb24oKSB7XG5cdFx0XHR2YXIgZUxsZW4sIGksIHJlY3QsIGF1dG9Mb2FkRWxlbSwgbG9hZGVkU29tZXRoaW5nLCBlbGVtRXhwYW5kLCBlbGVtTmVnYXRpdmVFeHBhbmQsIGVsZW1FeHBhbmRWYWwsXG5cdFx0XHRcdGJlZm9yZUV4cGFuZFZhbCwgZGVmYXVsdEV4cGFuZCwgcHJlbG9hZEV4cGFuZCwgaEZhYztcblx0XHRcdHZhciBsYXp5bG9hZEVsZW1zID0gbGF6eXNpemVzLmVsZW1lbnRzO1xuXG5cdFx0XHRpZigobG9hZE1vZGUgPSBsYXp5U2l6ZXNDZmcubG9hZE1vZGUpICYmIGlzTG9hZGluZyA8IDggJiYgKGVMbGVuID0gbGF6eWxvYWRFbGVtcy5sZW5ndGgpKXtcblxuXHRcdFx0XHRpID0gMDtcblxuXHRcdFx0XHRsb3dSdW5zKys7XG5cblx0XHRcdFx0Zm9yKDsgaSA8IGVMbGVuOyBpKyspe1xuXG5cdFx0XHRcdFx0aWYoIWxhenlsb2FkRWxlbXNbaV0gfHwgbGF6eWxvYWRFbGVtc1tpXS5fbGF6eVJhY2Upe2NvbnRpbnVlO31cblxuXHRcdFx0XHRcdGlmKCFzdXBwb3J0U2Nyb2xsIHx8IChsYXp5c2l6ZXMucHJlbWF0dXJlVW52ZWlsICYmIGxhenlzaXplcy5wcmVtYXR1cmVVbnZlaWwobGF6eWxvYWRFbGVtc1tpXSkpKXt1bnZlaWxFbGVtZW50KGxhenlsb2FkRWxlbXNbaV0pO2NvbnRpbnVlO31cblxuXHRcdFx0XHRcdGlmKCEoZWxlbUV4cGFuZFZhbCA9IGxhenlsb2FkRWxlbXNbaV1bX2dldEF0dHJpYnV0ZV0oJ2RhdGEtZXhwYW5kJykpIHx8ICEoZWxlbUV4cGFuZCA9IGVsZW1FeHBhbmRWYWwgKiAxKSl7XG5cdFx0XHRcdFx0XHRlbGVtRXhwYW5kID0gY3VycmVudEV4cGFuZDtcblx0XHRcdFx0XHR9XG5cblx0XHRcdFx0XHRpZiAoIWRlZmF1bHRFeHBhbmQpIHtcblx0XHRcdFx0XHRcdGRlZmF1bHRFeHBhbmQgPSAoIWxhenlTaXplc0NmZy5leHBhbmQgfHwgbGF6eVNpemVzQ2ZnLmV4cGFuZCA8IDEpID9cblx0XHRcdFx0XHRcdFx0ZG9jRWxlbS5jbGllbnRIZWlnaHQgPiA1MDAgJiYgZG9jRWxlbS5jbGllbnRXaWR0aCA+IDUwMCA/IDUwMCA6IDM3MCA6XG5cdFx0XHRcdFx0XHRcdGxhenlTaXplc0NmZy5leHBhbmQ7XG5cblx0XHRcdFx0XHRcdGxhenlzaXplcy5fZGVmRXggPSBkZWZhdWx0RXhwYW5kO1xuXG5cdFx0XHRcdFx0XHRwcmVsb2FkRXhwYW5kID0gZGVmYXVsdEV4cGFuZCAqIGxhenlTaXplc0NmZy5leHBGYWN0b3I7XG5cdFx0XHRcdFx0XHRoRmFjID0gbGF6eVNpemVzQ2ZnLmhGYWM7XG5cdFx0XHRcdFx0XHRpc0JvZHlIaWRkZW4gPSBudWxsO1xuXG5cdFx0XHRcdFx0XHRpZihjdXJyZW50RXhwYW5kIDwgcHJlbG9hZEV4cGFuZCAmJiBpc0xvYWRpbmcgPCAxICYmIGxvd1J1bnMgPiAyICYmIGxvYWRNb2RlID4gMiAmJiAhZG9jdW1lbnQuaGlkZGVuKXtcblx0XHRcdFx0XHRcdFx0Y3VycmVudEV4cGFuZCA9IHByZWxvYWRFeHBhbmQ7XG5cdFx0XHRcdFx0XHRcdGxvd1J1bnMgPSAwO1xuXHRcdFx0XHRcdFx0fSBlbHNlIGlmKGxvYWRNb2RlID4gMSAmJiBsb3dSdW5zID4gMSAmJiBpc0xvYWRpbmcgPCA2KXtcblx0XHRcdFx0XHRcdFx0Y3VycmVudEV4cGFuZCA9IGRlZmF1bHRFeHBhbmQ7XG5cdFx0XHRcdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHRcdFx0XHRjdXJyZW50RXhwYW5kID0gc2hyaW5rRXhwYW5kO1xuXHRcdFx0XHRcdFx0fVxuXHRcdFx0XHRcdH1cblxuXHRcdFx0XHRcdGlmKGJlZm9yZUV4cGFuZFZhbCAhPT0gZWxlbUV4cGFuZCl7XG5cdFx0XHRcdFx0XHRlTHZXID0gaW5uZXJXaWR0aCArIChlbGVtRXhwYW5kICogaEZhYyk7XG5cdFx0XHRcdFx0XHRlbHZIID0gaW5uZXJIZWlnaHQgKyBlbGVtRXhwYW5kO1xuXHRcdFx0XHRcdFx0ZWxlbU5lZ2F0aXZlRXhwYW5kID0gZWxlbUV4cGFuZCAqIC0xO1xuXHRcdFx0XHRcdFx0YmVmb3JlRXhwYW5kVmFsID0gZWxlbUV4cGFuZDtcblx0XHRcdFx0XHR9XG5cblx0XHRcdFx0XHRyZWN0ID0gbGF6eWxvYWRFbGVtc1tpXS5nZXRCb3VuZGluZ0NsaWVudFJlY3QoKTtcblxuXHRcdFx0XHRcdGlmICgoZUxib3R0b20gPSByZWN0LmJvdHRvbSkgPj0gZWxlbU5lZ2F0aXZlRXhwYW5kICYmXG5cdFx0XHRcdFx0XHQoZUx0b3AgPSByZWN0LnRvcCkgPD0gZWx2SCAmJlxuXHRcdFx0XHRcdFx0KGVMcmlnaHQgPSByZWN0LnJpZ2h0KSA+PSBlbGVtTmVnYXRpdmVFeHBhbmQgKiBoRmFjICYmXG5cdFx0XHRcdFx0XHQoZUxsZWZ0ID0gcmVjdC5sZWZ0KSA8PSBlTHZXICYmXG5cdFx0XHRcdFx0XHQoZUxib3R0b20gfHwgZUxyaWdodCB8fCBlTGxlZnQgfHwgZUx0b3ApICYmXG5cdFx0XHRcdFx0XHQobGF6eVNpemVzQ2ZnLmxvYWRIaWRkZW4gfHwgaXNWaXNpYmxlKGxhenlsb2FkRWxlbXNbaV0pKSAmJlxuXHRcdFx0XHRcdFx0KChpc0NvbXBsZXRlZCAmJiBpc0xvYWRpbmcgPCAzICYmICFlbGVtRXhwYW5kVmFsICYmIChsb2FkTW9kZSA8IDMgfHwgbG93UnVucyA8IDQpKSB8fCBpc05lc3RlZFZpc2libGUobGF6eWxvYWRFbGVtc1tpXSwgZWxlbUV4cGFuZCkpKXtcblx0XHRcdFx0XHRcdHVudmVpbEVsZW1lbnQobGF6eWxvYWRFbGVtc1tpXSk7XG5cdFx0XHRcdFx0XHRsb2FkZWRTb21ldGhpbmcgPSB0cnVlO1xuXHRcdFx0XHRcdFx0aWYoaXNMb2FkaW5nID4gOSl7YnJlYWs7fVxuXHRcdFx0XHRcdH0gZWxzZSBpZighbG9hZGVkU29tZXRoaW5nICYmIGlzQ29tcGxldGVkICYmICFhdXRvTG9hZEVsZW0gJiZcblx0XHRcdFx0XHRcdGlzTG9hZGluZyA8IDQgJiYgbG93UnVucyA8IDQgJiYgbG9hZE1vZGUgPiAyICYmXG5cdFx0XHRcdFx0XHQocHJlbG9hZEVsZW1zWzBdIHx8IGxhenlTaXplc0NmZy5wcmVsb2FkQWZ0ZXJMb2FkKSAmJlxuXHRcdFx0XHRcdFx0KHByZWxvYWRFbGVtc1swXSB8fCAoIWVsZW1FeHBhbmRWYWwgJiYgKChlTGJvdHRvbSB8fCBlTHJpZ2h0IHx8IGVMbGVmdCB8fCBlTHRvcCkgfHwgbGF6eWxvYWRFbGVtc1tpXVtfZ2V0QXR0cmlidXRlXShsYXp5U2l6ZXNDZmcuc2l6ZXNBdHRyKSAhPSAnYXV0bycpKSkpe1xuXHRcdFx0XHRcdFx0YXV0b0xvYWRFbGVtID0gcHJlbG9hZEVsZW1zWzBdIHx8IGxhenlsb2FkRWxlbXNbaV07XG5cdFx0XHRcdFx0fVxuXHRcdFx0XHR9XG5cblx0XHRcdFx0aWYoYXV0b0xvYWRFbGVtICYmICFsb2FkZWRTb21ldGhpbmcpe1xuXHRcdFx0XHRcdHVudmVpbEVsZW1lbnQoYXV0b0xvYWRFbGVtKTtcblx0XHRcdFx0fVxuXHRcdFx0fVxuXHRcdH07XG5cblx0XHR2YXIgdGhyb3R0bGVkQ2hlY2tFbGVtZW50cyA9IHRocm90dGxlKGNoZWNrRWxlbWVudHMpO1xuXG5cdFx0dmFyIHN3aXRjaExvYWRpbmdDbGFzcyA9IGZ1bmN0aW9uKGUpe1xuXHRcdFx0dmFyIGVsZW0gPSBlLnRhcmdldDtcblxuXHRcdFx0aWYgKGVsZW0uX2xhenlDYWNoZSkge1xuXHRcdFx0XHRkZWxldGUgZWxlbS5fbGF6eUNhY2hlO1xuXHRcdFx0XHRyZXR1cm47XG5cdFx0XHR9XG5cblx0XHRcdHJlc2V0UHJlbG9hZGluZyhlKTtcblx0XHRcdGFkZENsYXNzKGVsZW0sIGxhenlTaXplc0NmZy5sb2FkZWRDbGFzcyk7XG5cdFx0XHRyZW1vdmVDbGFzcyhlbGVtLCBsYXp5U2l6ZXNDZmcubG9hZGluZ0NsYXNzKTtcblx0XHRcdGFkZFJlbW92ZUxvYWRFdmVudHMoZWxlbSwgcmFmU3dpdGNoTG9hZGluZ0NsYXNzKTtcblx0XHRcdHRyaWdnZXJFdmVudChlbGVtLCAnbGF6eWxvYWRlZCcpO1xuXHRcdH07XG5cdFx0dmFyIHJhZmVkU3dpdGNoTG9hZGluZ0NsYXNzID0gckFGSXQoc3dpdGNoTG9hZGluZ0NsYXNzKTtcblx0XHR2YXIgcmFmU3dpdGNoTG9hZGluZ0NsYXNzID0gZnVuY3Rpb24oZSl7XG5cdFx0XHRyYWZlZFN3aXRjaExvYWRpbmdDbGFzcyh7dGFyZ2V0OiBlLnRhcmdldH0pO1xuXHRcdH07XG5cblx0XHR2YXIgY2hhbmdlSWZyYW1lU3JjID0gZnVuY3Rpb24oZWxlbSwgc3JjKXtcblx0XHRcdHZhciBsb2FkTW9kZSA9IGVsZW0uZ2V0QXR0cmlidXRlKCdkYXRhLWxvYWQtbW9kZScpIHx8IGxhenlTaXplc0NmZy5pZnJhbWVMb2FkTW9kZTtcblxuXHRcdFx0Ly8gbG9hZE1vZGUgY2FuIGJlIGFsc28gYSBzdHJpbmchXG5cdFx0XHRpZiAobG9hZE1vZGUgPT0gMCkge1xuXHRcdFx0XHRlbGVtLmNvbnRlbnRXaW5kb3cubG9jYXRpb24ucmVwbGFjZShzcmMpO1xuXHRcdFx0fSBlbHNlIGlmIChsb2FkTW9kZSA9PSAxKSB7XG5cdFx0XHRcdGVsZW0uc3JjID0gc3JjO1xuXHRcdFx0fVxuXHRcdH07XG5cblx0XHR2YXIgaGFuZGxlU291cmNlcyA9IGZ1bmN0aW9uKHNvdXJjZSl7XG5cdFx0XHR2YXIgY3VzdG9tTWVkaWE7XG5cblx0XHRcdHZhciBzb3VyY2VTcmNzZXQgPSBzb3VyY2VbX2dldEF0dHJpYnV0ZV0obGF6eVNpemVzQ2ZnLnNyY3NldEF0dHIpO1xuXG5cdFx0XHRpZiggKGN1c3RvbU1lZGlhID0gbGF6eVNpemVzQ2ZnLmN1c3RvbU1lZGlhW3NvdXJjZVtfZ2V0QXR0cmlidXRlXSgnZGF0YS1tZWRpYScpIHx8IHNvdXJjZVtfZ2V0QXR0cmlidXRlXSgnbWVkaWEnKV0pICl7XG5cdFx0XHRcdHNvdXJjZS5zZXRBdHRyaWJ1dGUoJ21lZGlhJywgY3VzdG9tTWVkaWEpO1xuXHRcdFx0fVxuXG5cdFx0XHRpZihzb3VyY2VTcmNzZXQpe1xuXHRcdFx0XHRzb3VyY2Uuc2V0QXR0cmlidXRlKCdzcmNzZXQnLCBzb3VyY2VTcmNzZXQpO1xuXHRcdFx0fVxuXHRcdH07XG5cblx0XHR2YXIgbGF6eVVudmVpbCA9IHJBRkl0KGZ1bmN0aW9uIChlbGVtLCBkZXRhaWwsIGlzQXV0bywgc2l6ZXMsIGlzSW1nKXtcblx0XHRcdHZhciBzcmMsIHNyY3NldCwgcGFyZW50LCBpc1BpY3R1cmUsIGV2ZW50LCBmaXJlc0xvYWQ7XG5cblx0XHRcdGlmKCEoZXZlbnQgPSB0cmlnZ2VyRXZlbnQoZWxlbSwgJ2xhenliZWZvcmV1bnZlaWwnLCBkZXRhaWwpKS5kZWZhdWx0UHJldmVudGVkKXtcblxuXHRcdFx0XHRpZihzaXplcyl7XG5cdFx0XHRcdFx0aWYoaXNBdXRvKXtcblx0XHRcdFx0XHRcdGFkZENsYXNzKGVsZW0sIGxhenlTaXplc0NmZy5hdXRvc2l6ZXNDbGFzcyk7XG5cdFx0XHRcdFx0fSBlbHNlIHtcblx0XHRcdFx0XHRcdGVsZW0uc2V0QXR0cmlidXRlKCdzaXplcycsIHNpemVzKTtcblx0XHRcdFx0XHR9XG5cdFx0XHRcdH1cblxuXHRcdFx0XHRzcmNzZXQgPSBlbGVtW19nZXRBdHRyaWJ1dGVdKGxhenlTaXplc0NmZy5zcmNzZXRBdHRyKTtcblx0XHRcdFx0c3JjID0gZWxlbVtfZ2V0QXR0cmlidXRlXShsYXp5U2l6ZXNDZmcuc3JjQXR0cik7XG5cblx0XHRcdFx0aWYoaXNJbWcpIHtcblx0XHRcdFx0XHRwYXJlbnQgPSBlbGVtLnBhcmVudE5vZGU7XG5cdFx0XHRcdFx0aXNQaWN0dXJlID0gcGFyZW50ICYmIHJlZ1BpY3R1cmUudGVzdChwYXJlbnQubm9kZU5hbWUgfHwgJycpO1xuXHRcdFx0XHR9XG5cblx0XHRcdFx0ZmlyZXNMb2FkID0gZGV0YWlsLmZpcmVzTG9hZCB8fCAoKCdzcmMnIGluIGVsZW0pICYmIChzcmNzZXQgfHwgc3JjIHx8IGlzUGljdHVyZSkpO1xuXG5cdFx0XHRcdGV2ZW50ID0ge3RhcmdldDogZWxlbX07XG5cblx0XHRcdFx0YWRkQ2xhc3MoZWxlbSwgbGF6eVNpemVzQ2ZnLmxvYWRpbmdDbGFzcyk7XG5cblx0XHRcdFx0aWYoZmlyZXNMb2FkKXtcblx0XHRcdFx0XHRjbGVhclRpbWVvdXQocmVzZXRQcmVsb2FkaW5nVGltZXIpO1xuXHRcdFx0XHRcdHJlc2V0UHJlbG9hZGluZ1RpbWVyID0gc2V0VGltZW91dChyZXNldFByZWxvYWRpbmcsIDI1MDApO1xuXHRcdFx0XHRcdGFkZFJlbW92ZUxvYWRFdmVudHMoZWxlbSwgcmFmU3dpdGNoTG9hZGluZ0NsYXNzLCB0cnVlKTtcblx0XHRcdFx0fVxuXG5cdFx0XHRcdGlmKGlzUGljdHVyZSl7XG5cdFx0XHRcdFx0Zm9yRWFjaC5jYWxsKHBhcmVudC5nZXRFbGVtZW50c0J5VGFnTmFtZSgnc291cmNlJyksIGhhbmRsZVNvdXJjZXMpO1xuXHRcdFx0XHR9XG5cblx0XHRcdFx0aWYoc3Jjc2V0KXtcblx0XHRcdFx0XHRlbGVtLnNldEF0dHJpYnV0ZSgnc3Jjc2V0Jywgc3Jjc2V0KTtcblx0XHRcdFx0fSBlbHNlIGlmKHNyYyAmJiAhaXNQaWN0dXJlKXtcblx0XHRcdFx0XHRpZihyZWdJZnJhbWUudGVzdChlbGVtLm5vZGVOYW1lKSl7XG5cdFx0XHRcdFx0XHRjaGFuZ2VJZnJhbWVTcmMoZWxlbSwgc3JjKTtcblx0XHRcdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHRcdFx0ZWxlbS5zcmMgPSBzcmM7XG5cdFx0XHRcdFx0fVxuXHRcdFx0XHR9XG5cblx0XHRcdFx0aWYoaXNJbWcgJiYgKHNyY3NldCB8fCBpc1BpY3R1cmUpKXtcblx0XHRcdFx0XHR1cGRhdGVQb2x5ZmlsbChlbGVtLCB7c3JjOiBzcmN9KTtcblx0XHRcdFx0fVxuXHRcdFx0fVxuXG5cdFx0XHRpZihlbGVtLl9sYXp5UmFjZSl7XG5cdFx0XHRcdGRlbGV0ZSBlbGVtLl9sYXp5UmFjZTtcblx0XHRcdH1cblx0XHRcdHJlbW92ZUNsYXNzKGVsZW0sIGxhenlTaXplc0NmZy5sYXp5Q2xhc3MpO1xuXG5cdFx0XHRyQUYoZnVuY3Rpb24oKXtcblx0XHRcdFx0Ly8gUGFydCBvZiB0aGlzIGNhbiBiZSByZW1vdmVkIGFzIHNvb24gYXMgdGhpcyBmaXggaXMgb2xkZXI6IGh0dHBzOi8vYnVncy5jaHJvbWl1bS5vcmcvcC9jaHJvbWl1bS9pc3N1ZXMvZGV0YWlsP2lkPTc3MzEgKDIwMTUpXG5cdFx0XHRcdHZhciBpc0xvYWRlZCA9IGVsZW0uY29tcGxldGUgJiYgZWxlbS5uYXR1cmFsV2lkdGggPiAxO1xuXG5cdFx0XHRcdGlmKCAhZmlyZXNMb2FkIHx8IGlzTG9hZGVkKXtcblx0XHRcdFx0XHRpZiAoaXNMb2FkZWQpIHtcblx0XHRcdFx0XHRcdGFkZENsYXNzKGVsZW0sIGxhenlTaXplc0NmZy5mYXN0TG9hZGVkQ2xhc3MpO1xuXHRcdFx0XHRcdH1cblx0XHRcdFx0XHRzd2l0Y2hMb2FkaW5nQ2xhc3MoZXZlbnQpO1xuXHRcdFx0XHRcdGVsZW0uX2xhenlDYWNoZSA9IHRydWU7XG5cdFx0XHRcdFx0c2V0VGltZW91dChmdW5jdGlvbigpe1xuXHRcdFx0XHRcdFx0aWYgKCdfbGF6eUNhY2hlJyBpbiBlbGVtKSB7XG5cdFx0XHRcdFx0XHRcdGRlbGV0ZSBlbGVtLl9sYXp5Q2FjaGU7XG5cdFx0XHRcdFx0XHR9XG5cdFx0XHRcdFx0fSwgOSk7XG5cdFx0XHRcdH1cblx0XHRcdFx0aWYgKGVsZW0ubG9hZGluZyA9PSAnbGF6eScpIHtcblx0XHRcdFx0XHRpc0xvYWRpbmctLTtcblx0XHRcdFx0fVxuXHRcdFx0fSwgdHJ1ZSk7XG5cdFx0fSk7XG5cblx0XHQvKipcblx0XHQgKlxuXHRcdCAqIEBwYXJhbSBlbGVtIHsgRWxlbWVudCB9XG5cdFx0ICovXG5cdFx0dmFyIHVudmVpbEVsZW1lbnQgPSBmdW5jdGlvbiAoZWxlbSl7XG5cdFx0XHRpZiAoZWxlbS5fbGF6eVJhY2UpIHtyZXR1cm47fVxuXHRcdFx0dmFyIGRldGFpbDtcblxuXHRcdFx0dmFyIGlzSW1nID0gcmVnSW1nLnRlc3QoZWxlbS5ub2RlTmFtZSk7XG5cblx0XHRcdC8vYWxsb3cgdXNpbmcgc2l6ZXM9XCJhdXRvXCIsIGJ1dCBkb24ndCB1c2UuIGl0J3MgaW52YWxpZC4gVXNlIGRhdGEtc2l6ZXM9XCJhdXRvXCIgb3IgYSB2YWxpZCB2YWx1ZSBmb3Igc2l6ZXMgaW5zdGVhZCAoaS5lLjogc2l6ZXM9XCI4MHZ3XCIpXG5cdFx0XHR2YXIgc2l6ZXMgPSBpc0ltZyAmJiAoZWxlbVtfZ2V0QXR0cmlidXRlXShsYXp5U2l6ZXNDZmcuc2l6ZXNBdHRyKSB8fCBlbGVtW19nZXRBdHRyaWJ1dGVdKCdzaXplcycpKTtcblx0XHRcdHZhciBpc0F1dG8gPSBzaXplcyA9PSAnYXV0byc7XG5cblx0XHRcdGlmKCAoaXNBdXRvIHx8ICFpc0NvbXBsZXRlZCkgJiYgaXNJbWcgJiYgKGVsZW1bX2dldEF0dHJpYnV0ZV0oJ3NyYycpIHx8IGVsZW0uc3Jjc2V0KSAmJiAhZWxlbS5jb21wbGV0ZSAmJiAhaGFzQ2xhc3MoZWxlbSwgbGF6eVNpemVzQ2ZnLmVycm9yQ2xhc3MpICYmIGhhc0NsYXNzKGVsZW0sIGxhenlTaXplc0NmZy5sYXp5Q2xhc3MpKXtyZXR1cm47fVxuXG5cdFx0XHRkZXRhaWwgPSB0cmlnZ2VyRXZlbnQoZWxlbSwgJ2xhenl1bnZlaWxyZWFkJykuZGV0YWlsO1xuXG5cdFx0XHRpZihpc0F1dG8pe1xuXHRcdFx0XHQgYXV0b1NpemVyLnVwZGF0ZUVsZW0oZWxlbSwgdHJ1ZSwgZWxlbS5vZmZzZXRXaWR0aCk7XG5cdFx0XHR9XG5cblx0XHRcdGVsZW0uX2xhenlSYWNlID0gdHJ1ZTtcblx0XHRcdGlzTG9hZGluZysrO1xuXG5cdFx0XHRsYXp5VW52ZWlsKGVsZW0sIGRldGFpbCwgaXNBdXRvLCBzaXplcywgaXNJbWcpO1xuXHRcdH07XG5cblx0XHR2YXIgYWZ0ZXJTY3JvbGwgPSBkZWJvdW5jZShmdW5jdGlvbigpe1xuXHRcdFx0bGF6eVNpemVzQ2ZnLmxvYWRNb2RlID0gMztcblx0XHRcdHRocm90dGxlZENoZWNrRWxlbWVudHMoKTtcblx0XHR9KTtcblxuXHRcdHZhciBhbHRMb2FkbW9kZVNjcm9sbExpc3RuZXIgPSBmdW5jdGlvbigpe1xuXHRcdFx0aWYobGF6eVNpemVzQ2ZnLmxvYWRNb2RlID09IDMpe1xuXHRcdFx0XHRsYXp5U2l6ZXNDZmcubG9hZE1vZGUgPSAyO1xuXHRcdFx0fVxuXHRcdFx0YWZ0ZXJTY3JvbGwoKTtcblx0XHR9O1xuXG5cdFx0dmFyIG9ubG9hZCA9IGZ1bmN0aW9uKCl7XG5cdFx0XHRpZihpc0NvbXBsZXRlZCl7cmV0dXJuO31cblx0XHRcdGlmKERhdGUubm93KCkgLSBzdGFydGVkIDwgOTk5KXtcblx0XHRcdFx0c2V0VGltZW91dChvbmxvYWQsIDk5OSk7XG5cdFx0XHRcdHJldHVybjtcblx0XHRcdH1cblxuXG5cdFx0XHRpc0NvbXBsZXRlZCA9IHRydWU7XG5cblx0XHRcdGxhenlTaXplc0NmZy5sb2FkTW9kZSA9IDM7XG5cblx0XHRcdHRocm90dGxlZENoZWNrRWxlbWVudHMoKTtcblxuXHRcdFx0YWRkRXZlbnRMaXN0ZW5lcignc2Nyb2xsJywgYWx0TG9hZG1vZGVTY3JvbGxMaXN0bmVyLCB0cnVlKTtcblx0XHR9O1xuXG5cdFx0cmV0dXJuIHtcblx0XHRcdF86IGZ1bmN0aW9uKCl7XG5cdFx0XHRcdHN0YXJ0ZWQgPSBEYXRlLm5vdygpO1xuXG5cdFx0XHRcdGxhenlzaXplcy5lbGVtZW50cyA9IGRvY3VtZW50LmdldEVsZW1lbnRzQnlDbGFzc05hbWUobGF6eVNpemVzQ2ZnLmxhenlDbGFzcyk7XG5cdFx0XHRcdHByZWxvYWRFbGVtcyA9IGRvY3VtZW50LmdldEVsZW1lbnRzQnlDbGFzc05hbWUobGF6eVNpemVzQ2ZnLmxhenlDbGFzcyArICcgJyArIGxhenlTaXplc0NmZy5wcmVsb2FkQ2xhc3MpO1xuXG5cdFx0XHRcdGFkZEV2ZW50TGlzdGVuZXIoJ3Njcm9sbCcsIHRocm90dGxlZENoZWNrRWxlbWVudHMsIHRydWUpO1xuXG5cdFx0XHRcdGFkZEV2ZW50TGlzdGVuZXIoJ3Jlc2l6ZScsIHRocm90dGxlZENoZWNrRWxlbWVudHMsIHRydWUpO1xuXG5cdFx0XHRcdGFkZEV2ZW50TGlzdGVuZXIoJ3BhZ2VzaG93JywgZnVuY3Rpb24gKGUpIHtcblx0XHRcdFx0XHRpZiAoZS5wZXJzaXN0ZWQpIHtcblx0XHRcdFx0XHRcdHZhciBsb2FkaW5nRWxlbWVudHMgPSBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKCcuJyArIGxhenlTaXplc0NmZy5sb2FkaW5nQ2xhc3MpO1xuXG5cdFx0XHRcdFx0XHRpZiAobG9hZGluZ0VsZW1lbnRzLmxlbmd0aCAmJiBsb2FkaW5nRWxlbWVudHMuZm9yRWFjaCkge1xuXHRcdFx0XHRcdFx0XHRyZXF1ZXN0QW5pbWF0aW9uRnJhbWUoZnVuY3Rpb24gKCkge1xuXHRcdFx0XHRcdFx0XHRcdGxvYWRpbmdFbGVtZW50cy5mb3JFYWNoKCBmdW5jdGlvbiAoaW1nKSB7XG5cdFx0XHRcdFx0XHRcdFx0XHRpZiAoaW1nLmNvbXBsZXRlKSB7XG5cdFx0XHRcdFx0XHRcdFx0XHRcdHVudmVpbEVsZW1lbnQoaW1nKTtcblx0XHRcdFx0XHRcdFx0XHRcdH1cblx0XHRcdFx0XHRcdFx0XHR9KTtcblx0XHRcdFx0XHRcdFx0fSk7XG5cdFx0XHRcdFx0XHR9XG5cdFx0XHRcdFx0fVxuXHRcdFx0XHR9KTtcblxuXHRcdFx0XHRpZih3aW5kb3cuTXV0YXRpb25PYnNlcnZlcil7XG5cdFx0XHRcdFx0bmV3IE11dGF0aW9uT2JzZXJ2ZXIoIHRocm90dGxlZENoZWNrRWxlbWVudHMgKS5vYnNlcnZlKCBkb2NFbGVtLCB7Y2hpbGRMaXN0OiB0cnVlLCBzdWJ0cmVlOiB0cnVlLCBhdHRyaWJ1dGVzOiB0cnVlfSApO1xuXHRcdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHRcdGRvY0VsZW1bX2FkZEV2ZW50TGlzdGVuZXJdKCdET01Ob2RlSW5zZXJ0ZWQnLCB0aHJvdHRsZWRDaGVja0VsZW1lbnRzLCB0cnVlKTtcblx0XHRcdFx0XHRkb2NFbGVtW19hZGRFdmVudExpc3RlbmVyXSgnRE9NQXR0ck1vZGlmaWVkJywgdGhyb3R0bGVkQ2hlY2tFbGVtZW50cywgdHJ1ZSk7XG5cdFx0XHRcdFx0c2V0SW50ZXJ2YWwodGhyb3R0bGVkQ2hlY2tFbGVtZW50cywgOTk5KTtcblx0XHRcdFx0fVxuXG5cdFx0XHRcdGFkZEV2ZW50TGlzdGVuZXIoJ2hhc2hjaGFuZ2UnLCB0aHJvdHRsZWRDaGVja0VsZW1lbnRzLCB0cnVlKTtcblxuXHRcdFx0XHQvLywgJ2Z1bGxzY3JlZW5jaGFuZ2UnXG5cdFx0XHRcdFsnZm9jdXMnLCAnbW91c2VvdmVyJywgJ2NsaWNrJywgJ2xvYWQnLCAndHJhbnNpdGlvbmVuZCcsICdhbmltYXRpb25lbmQnXS5mb3JFYWNoKGZ1bmN0aW9uKG5hbWUpe1xuXHRcdFx0XHRcdGRvY3VtZW50W19hZGRFdmVudExpc3RlbmVyXShuYW1lLCB0aHJvdHRsZWRDaGVja0VsZW1lbnRzLCB0cnVlKTtcblx0XHRcdFx0fSk7XG5cblx0XHRcdFx0aWYoKC9kJHxeYy8udGVzdChkb2N1bWVudC5yZWFkeVN0YXRlKSkpe1xuXHRcdFx0XHRcdG9ubG9hZCgpO1xuXHRcdFx0XHR9IGVsc2Uge1xuXHRcdFx0XHRcdGFkZEV2ZW50TGlzdGVuZXIoJ2xvYWQnLCBvbmxvYWQpO1xuXHRcdFx0XHRcdGRvY3VtZW50W19hZGRFdmVudExpc3RlbmVyXSgnRE9NQ29udGVudExvYWRlZCcsIHRocm90dGxlZENoZWNrRWxlbWVudHMpO1xuXHRcdFx0XHRcdHNldFRpbWVvdXQob25sb2FkLCAyMDAwMCk7XG5cdFx0XHRcdH1cblxuXHRcdFx0XHRpZihsYXp5c2l6ZXMuZWxlbWVudHMubGVuZ3RoKXtcblx0XHRcdFx0XHRjaGVja0VsZW1lbnRzKCk7XG5cdFx0XHRcdFx0ckFGLl9sc0ZsdXNoKCk7XG5cdFx0XHRcdH0gZWxzZSB7XG5cdFx0XHRcdFx0dGhyb3R0bGVkQ2hlY2tFbGVtZW50cygpO1xuXHRcdFx0XHR9XG5cdFx0XHR9LFxuXHRcdFx0Y2hlY2tFbGVtczogdGhyb3R0bGVkQ2hlY2tFbGVtZW50cyxcblx0XHRcdHVudmVpbDogdW52ZWlsRWxlbWVudCxcblx0XHRcdF9hTFNMOiBhbHRMb2FkbW9kZVNjcm9sbExpc3RuZXIsXG5cdFx0fTtcblx0fSkoKTtcblxuXG5cdHZhciBhdXRvU2l6ZXIgPSAoZnVuY3Rpb24oKXtcblx0XHR2YXIgYXV0b3NpemVzRWxlbXM7XG5cblx0XHR2YXIgc2l6ZUVsZW1lbnQgPSByQUZJdChmdW5jdGlvbihlbGVtLCBwYXJlbnQsIGV2ZW50LCB3aWR0aCl7XG5cdFx0XHR2YXIgc291cmNlcywgaSwgbGVuO1xuXHRcdFx0ZWxlbS5fbGF6eXNpemVzV2lkdGggPSB3aWR0aDtcblx0XHRcdHdpZHRoICs9ICdweCc7XG5cblx0XHRcdGVsZW0uc2V0QXR0cmlidXRlKCdzaXplcycsIHdpZHRoKTtcblxuXHRcdFx0aWYocmVnUGljdHVyZS50ZXN0KHBhcmVudC5ub2RlTmFtZSB8fCAnJykpe1xuXHRcdFx0XHRzb3VyY2VzID0gcGFyZW50LmdldEVsZW1lbnRzQnlUYWdOYW1lKCdzb3VyY2UnKTtcblx0XHRcdFx0Zm9yKGkgPSAwLCBsZW4gPSBzb3VyY2VzLmxlbmd0aDsgaSA8IGxlbjsgaSsrKXtcblx0XHRcdFx0XHRzb3VyY2VzW2ldLnNldEF0dHJpYnV0ZSgnc2l6ZXMnLCB3aWR0aCk7XG5cdFx0XHRcdH1cblx0XHRcdH1cblxuXHRcdFx0aWYoIWV2ZW50LmRldGFpbC5kYXRhQXR0cil7XG5cdFx0XHRcdHVwZGF0ZVBvbHlmaWxsKGVsZW0sIGV2ZW50LmRldGFpbCk7XG5cdFx0XHR9XG5cdFx0fSk7XG5cdFx0LyoqXG5cdFx0ICpcblx0XHQgKiBAcGFyYW0gZWxlbSB7RWxlbWVudH1cblx0XHQgKiBAcGFyYW0gZGF0YUF0dHJcblx0XHQgKiBAcGFyYW0gW3dpZHRoXSB7IG51bWJlciB9XG5cdFx0ICovXG5cdFx0dmFyIGdldFNpemVFbGVtZW50ID0gZnVuY3Rpb24gKGVsZW0sIGRhdGFBdHRyLCB3aWR0aCl7XG5cdFx0XHR2YXIgZXZlbnQ7XG5cdFx0XHR2YXIgcGFyZW50ID0gZWxlbS5wYXJlbnROb2RlO1xuXG5cdFx0XHRpZihwYXJlbnQpe1xuXHRcdFx0XHR3aWR0aCA9IGdldFdpZHRoKGVsZW0sIHBhcmVudCwgd2lkdGgpO1xuXHRcdFx0XHRldmVudCA9IHRyaWdnZXJFdmVudChlbGVtLCAnbGF6eWJlZm9yZXNpemVzJywge3dpZHRoOiB3aWR0aCwgZGF0YUF0dHI6ICEhZGF0YUF0dHJ9KTtcblxuXHRcdFx0XHRpZighZXZlbnQuZGVmYXVsdFByZXZlbnRlZCl7XG5cdFx0XHRcdFx0d2lkdGggPSBldmVudC5kZXRhaWwud2lkdGg7XG5cblx0XHRcdFx0XHRpZih3aWR0aCAmJiB3aWR0aCAhPT0gZWxlbS5fbGF6eXNpemVzV2lkdGgpe1xuXHRcdFx0XHRcdFx0c2l6ZUVsZW1lbnQoZWxlbSwgcGFyZW50LCBldmVudCwgd2lkdGgpO1xuXHRcdFx0XHRcdH1cblx0XHRcdFx0fVxuXHRcdFx0fVxuXHRcdH07XG5cblx0XHR2YXIgdXBkYXRlRWxlbWVudHNTaXplcyA9IGZ1bmN0aW9uKCl7XG5cdFx0XHR2YXIgaTtcblx0XHRcdHZhciBsZW4gPSBhdXRvc2l6ZXNFbGVtcy5sZW5ndGg7XG5cdFx0XHRpZihsZW4pe1xuXHRcdFx0XHRpID0gMDtcblxuXHRcdFx0XHRmb3IoOyBpIDwgbGVuOyBpKyspe1xuXHRcdFx0XHRcdGdldFNpemVFbGVtZW50KGF1dG9zaXplc0VsZW1zW2ldKTtcblx0XHRcdFx0fVxuXHRcdFx0fVxuXHRcdH07XG5cblx0XHR2YXIgZGVib3VuY2VkVXBkYXRlRWxlbWVudHNTaXplcyA9IGRlYm91bmNlKHVwZGF0ZUVsZW1lbnRzU2l6ZXMpO1xuXG5cdFx0cmV0dXJuIHtcblx0XHRcdF86IGZ1bmN0aW9uKCl7XG5cdFx0XHRcdGF1dG9zaXplc0VsZW1zID0gZG9jdW1lbnQuZ2V0RWxlbWVudHNCeUNsYXNzTmFtZShsYXp5U2l6ZXNDZmcuYXV0b3NpemVzQ2xhc3MpO1xuXHRcdFx0XHRhZGRFdmVudExpc3RlbmVyKCdyZXNpemUnLCBkZWJvdW5jZWRVcGRhdGVFbGVtZW50c1NpemVzKTtcblx0XHRcdH0sXG5cdFx0XHRjaGVja0VsZW1zOiBkZWJvdW5jZWRVcGRhdGVFbGVtZW50c1NpemVzLFxuXHRcdFx0dXBkYXRlRWxlbTogZ2V0U2l6ZUVsZW1lbnRcblx0XHR9O1xuXHR9KSgpO1xuXG5cdHZhciBpbml0ID0gZnVuY3Rpb24oKXtcblx0XHRpZighaW5pdC5pICYmIGRvY3VtZW50LmdldEVsZW1lbnRzQnlDbGFzc05hbWUpe1xuXHRcdFx0aW5pdC5pID0gdHJ1ZTtcblx0XHRcdGF1dG9TaXplci5fKCk7XG5cdFx0XHRsb2FkZXIuXygpO1xuXHRcdH1cblx0fTtcblxuXHRzZXRUaW1lb3V0KGZ1bmN0aW9uKCl7XG5cdFx0aWYobGF6eVNpemVzQ2ZnLmluaXQpe1xuXHRcdFx0aW5pdCgpO1xuXHRcdH1cblx0fSk7XG5cblx0bGF6eXNpemVzID0ge1xuXHRcdC8qKlxuXHRcdCAqIEB0eXBlIHsgTGF6eVNpemVzQ29uZmlnUGFydGlhbCB9XG5cdFx0ICovXG5cdFx0Y2ZnOiBsYXp5U2l6ZXNDZmcsXG5cdFx0YXV0b1NpemVyOiBhdXRvU2l6ZXIsXG5cdFx0bG9hZGVyOiBsb2FkZXIsXG5cdFx0aW5pdDogaW5pdCxcblx0XHR1UDogdXBkYXRlUG9seWZpbGwsXG5cdFx0YUM6IGFkZENsYXNzLFxuXHRcdHJDOiByZW1vdmVDbGFzcyxcblx0XHRoQzogaGFzQ2xhc3MsXG5cdFx0ZmlyZTogdHJpZ2dlckV2ZW50LFxuXHRcdGdXOiBnZXRXaWR0aCxcblx0XHRyQUY6IHJBRixcblx0fTtcblxuXHRyZXR1cm4gbGF6eXNpemVzO1xufVxuKSk7XG4iLCAiKGZ1bmN0aW9uKHdpbmRvdywgZmFjdG9yeSkge1xuXHR2YXIgZ2xvYmFsSW5zdGFsbCA9IGZ1bmN0aW9uKCl7XG5cdFx0ZmFjdG9yeSh3aW5kb3cubGF6eVNpemVzKTtcblx0XHR3aW5kb3cucmVtb3ZlRXZlbnRMaXN0ZW5lcignbGF6eXVudmVpbHJlYWQnLCBnbG9iYWxJbnN0YWxsLCB0cnVlKTtcblx0fTtcblxuXHRmYWN0b3J5ID0gZmFjdG9yeS5iaW5kKG51bGwsIHdpbmRvdywgd2luZG93LmRvY3VtZW50KTtcblxuXHRpZih0eXBlb2YgbW9kdWxlID09ICdvYmplY3QnICYmIG1vZHVsZS5leHBvcnRzKXtcblx0XHRmYWN0b3J5KHJlcXVpcmUoJ2xhenlzaXplcycpKTtcblx0fSBlbHNlIGlmICh0eXBlb2YgZGVmaW5lID09ICdmdW5jdGlvbicgJiYgZGVmaW5lLmFtZCkge1xuXHRcdGRlZmluZShbJ2xhenlzaXplcyddLCBmYWN0b3J5KTtcblx0fSBlbHNlIGlmKHdpbmRvdy5sYXp5U2l6ZXMpIHtcblx0XHRnbG9iYWxJbnN0YWxsKCk7XG5cdH0gZWxzZSB7XG5cdFx0d2luZG93LmFkZEV2ZW50TGlzdGVuZXIoJ2xhenl1bnZlaWxyZWFkJywgZ2xvYmFsSW5zdGFsbCwgdHJ1ZSk7XG5cdH1cbn0od2luZG93LCBmdW5jdGlvbih3aW5kb3csIGRvY3VtZW50LCBsYXp5U2l6ZXMpIHtcblx0J3VzZSBzdHJpY3QnO1xuXG5cdHZhciBpbWdTdXBwb3J0ID0gJ2xvYWRpbmcnIGluIEhUTUxJbWFnZUVsZW1lbnQucHJvdG90eXBlO1xuXHR2YXIgaWZyYW1lU3VwcG9ydCA9ICdsb2FkaW5nJyBpbiBIVE1MSUZyYW1lRWxlbWVudC5wcm90b3R5cGU7XG5cdHZhciBpc0NvbmZpZ1NldCA9IGZhbHNlO1xuXHR2YXIgb2xkUHJlbWF0dXJlVW52ZWlsID0gbGF6eVNpemVzLnByZW1hdHVyZVVudmVpbDtcblx0dmFyIGNmZyA9IGxhenlTaXplcy5jZmc7XG5cdHZhciBsaXN0ZW5lck1hcCA9IHtcblx0XHRmb2N1czogMSxcblx0XHRtb3VzZW92ZXI6IDEsXG5cdFx0Y2xpY2s6IDEsXG5cdFx0bG9hZDogMSxcblx0XHR0cmFuc2l0aW9uZW5kOiAxLFxuXHRcdGFuaW1hdGlvbmVuZDogMSxcblx0XHRzY3JvbGw6IDEsXG5cdFx0cmVzaXplOiAxLFxuXHR9O1xuXG5cdGlmICghY2ZnLm5hdGl2ZUxvYWRpbmcpIHtcblx0XHRjZmcubmF0aXZlTG9hZGluZyA9IHt9O1xuXHR9XG5cblx0aWYgKCF3aW5kb3cuYWRkRXZlbnRMaXN0ZW5lciB8fCAhd2luZG93Lk11dGF0aW9uT2JzZXJ2ZXIgfHwgKCFpbWdTdXBwb3J0ICYmICFpZnJhbWVTdXBwb3J0KSkge1xuXHRcdHJldHVybjtcblx0fVxuXG5cdGZ1bmN0aW9uIGRpc2FibGVFdmVudHMoKSB7XG5cdFx0dmFyIGxvYWRlciA9IGxhenlTaXplcy5sb2FkZXI7XG5cdFx0dmFyIHRocm90dGxlZENoZWNrRWxlbWVudHMgPSBsb2FkZXIuY2hlY2tFbGVtcztcblx0XHR2YXIgcmVtb3ZlQUxTTCA9IGZ1bmN0aW9uKCl7XG5cdFx0XHRzZXRUaW1lb3V0KGZ1bmN0aW9uKCl7XG5cdFx0XHRcdHdpbmRvdy5yZW1vdmVFdmVudExpc3RlbmVyKCdzY3JvbGwnLCBsb2FkZXIuX2FMU0wsIHRydWUpO1xuXHRcdFx0fSwgMTAwMCk7XG5cdFx0fTtcblx0XHR2YXIgY3VycmVudExpc3RlbmVyTWFwID0gdHlwZW9mIGNmZy5uYXRpdmVMb2FkaW5nLmRpc2FibGVMaXN0ZW5lcnMgPT0gJ29iamVjdCcgP1xuXHRcdFx0Y2ZnLm5hdGl2ZUxvYWRpbmcuZGlzYWJsZUxpc3RlbmVycyA6XG5cdFx0XHRsaXN0ZW5lck1hcDtcblxuXHRcdGlmIChjdXJyZW50TGlzdGVuZXJNYXAuc2Nyb2xsKSB7XG5cdFx0XHR3aW5kb3cuYWRkRXZlbnRMaXN0ZW5lcignbG9hZCcsIHJlbW92ZUFMU0wpO1xuXHRcdFx0cmVtb3ZlQUxTTCgpO1xuXG5cdFx0XHR3aW5kb3cucmVtb3ZlRXZlbnRMaXN0ZW5lcignc2Nyb2xsJywgdGhyb3R0bGVkQ2hlY2tFbGVtZW50cywgdHJ1ZSk7XG5cdFx0fVxuXG5cdFx0aWYgKGN1cnJlbnRMaXN0ZW5lck1hcC5yZXNpemUpIHtcblx0XHRcdHdpbmRvdy5yZW1vdmVFdmVudExpc3RlbmVyKCdyZXNpemUnLCB0aHJvdHRsZWRDaGVja0VsZW1lbnRzLCB0cnVlKTtcblx0XHR9XG5cblx0XHRPYmplY3Qua2V5cyhjdXJyZW50TGlzdGVuZXJNYXApLmZvckVhY2goZnVuY3Rpb24obmFtZSkge1xuXHRcdFx0aWYgKGN1cnJlbnRMaXN0ZW5lck1hcFtuYW1lXSkge1xuXHRcdFx0XHRkb2N1bWVudC5yZW1vdmVFdmVudExpc3RlbmVyKG5hbWUsIHRocm90dGxlZENoZWNrRWxlbWVudHMsIHRydWUpO1xuXHRcdFx0fVxuXHRcdH0pO1xuXHR9XG5cblx0ZnVuY3Rpb24gcnVuQ29uZmlnKCkge1xuXHRcdGlmIChpc0NvbmZpZ1NldCkge3JldHVybjt9XG5cdFx0aXNDb25maWdTZXQgPSB0cnVlO1xuXG5cdFx0aWYgKGltZ1N1cHBvcnQgJiYgaWZyYW1lU3VwcG9ydCAmJiBjZmcubmF0aXZlTG9hZGluZy5kaXNhYmxlTGlzdGVuZXJzKSB7XG5cdFx0XHRpZiAoY2ZnLm5hdGl2ZUxvYWRpbmcuZGlzYWJsZUxpc3RlbmVycyA9PT0gdHJ1ZSkge1xuXHRcdFx0XHRjZmcubmF0aXZlTG9hZGluZy5zZXRMb2FkaW5nQXR0cmlidXRlID0gdHJ1ZTtcblx0XHRcdH1cblxuXHRcdFx0ZGlzYWJsZUV2ZW50cygpO1xuXHRcdH1cblxuXHRcdGlmIChjZmcubmF0aXZlTG9hZGluZy5zZXRMb2FkaW5nQXR0cmlidXRlKSB7XG5cdFx0XHR3aW5kb3cuYWRkRXZlbnRMaXN0ZW5lcignbGF6eWJlZm9yZXVudmVpbCcsIGZ1bmN0aW9uKGUpe1xuXHRcdFx0XHR2YXIgZWxlbWVudCA9IGUudGFyZ2V0O1xuXG5cdFx0XHRcdGlmICgnbG9hZGluZycgaW4gZWxlbWVudCAmJiAhZWxlbWVudC5nZXRBdHRyaWJ1dGUoJ2xvYWRpbmcnKSkge1xuXHRcdFx0XHRcdGVsZW1lbnQuc2V0QXR0cmlidXRlKCdsb2FkaW5nJywgJ2xhenknKTtcblx0XHRcdFx0fVxuXHRcdFx0fSwgdHJ1ZSk7XG5cdFx0fVxuXHR9XG5cblx0bGF6eVNpemVzLnByZW1hdHVyZVVudmVpbCA9IGZ1bmN0aW9uIHByZW1hdHVyZVVudmVpbChlbGVtZW50KSB7XG5cblx0XHRpZiAoIWlzQ29uZmlnU2V0KSB7XG5cdFx0XHRydW5Db25maWcoKTtcblx0XHR9XG5cblx0XHRpZiAoJ2xvYWRpbmcnIGluIGVsZW1lbnQgJiZcblx0XHRcdChjZmcubmF0aXZlTG9hZGluZy5zZXRMb2FkaW5nQXR0cmlidXRlIHx8IGVsZW1lbnQuZ2V0QXR0cmlidXRlKCdsb2FkaW5nJykpICYmXG5cdFx0XHQoZWxlbWVudC5nZXRBdHRyaWJ1dGUoJ2RhdGEtc2l6ZXMnKSAhPSAnYXV0bycgfHwgZWxlbWVudC5vZmZzZXRXaWR0aCkpIHtcblx0XHRcdHJldHVybiB0cnVlO1xuXHRcdH1cblxuXHRcdGlmIChvbGRQcmVtYXR1cmVVbnZlaWwpIHtcblx0XHRcdHJldHVybiBvbGRQcmVtYXR1cmVVbnZlaWwoZWxlbWVudCk7XG5cdFx0fVxuXHR9O1xuXG59KSk7XG4iLCAiLyohXG4gKiBjbGlwYm9hcmQuanMgdjIuMC4xMVxuICogaHR0cHM6Ly9jbGlwYm9hcmRqcy5jb20vXG4gKlxuICogTGljZW5zZWQgTUlUIFx1MDBBOSBaZW5vIFJvY2hhXG4gKi9cbihmdW5jdGlvbiB3ZWJwYWNrVW5pdmVyc2FsTW9kdWxlRGVmaW5pdGlvbihyb290LCBmYWN0b3J5KSB7XG5cdGlmKHR5cGVvZiBleHBvcnRzID09PSAnb2JqZWN0JyAmJiB0eXBlb2YgbW9kdWxlID09PSAnb2JqZWN0Jylcblx0XHRtb2R1bGUuZXhwb3J0cyA9IGZhY3RvcnkoKTtcblx0ZWxzZSBpZih0eXBlb2YgZGVmaW5lID09PSAnZnVuY3Rpb24nICYmIGRlZmluZS5hbWQpXG5cdFx0ZGVmaW5lKFtdLCBmYWN0b3J5KTtcblx0ZWxzZSBpZih0eXBlb2YgZXhwb3J0cyA9PT0gJ29iamVjdCcpXG5cdFx0ZXhwb3J0c1tcIkNsaXBib2FyZEpTXCJdID0gZmFjdG9yeSgpO1xuXHRlbHNlXG5cdFx0cm9vdFtcIkNsaXBib2FyZEpTXCJdID0gZmFjdG9yeSgpO1xufSkodGhpcywgZnVuY3Rpb24oKSB7XG5yZXR1cm4gLyoqKioqKi8gKGZ1bmN0aW9uKCkgeyAvLyB3ZWJwYWNrQm9vdHN0cmFwXG4vKioqKioqLyBcdHZhciBfX3dlYnBhY2tfbW9kdWxlc19fID0gKHtcblxuLyoqKi8gNjg2OlxuLyoqKi8gKGZ1bmN0aW9uKF9fdW51c2VkX3dlYnBhY2tfbW9kdWxlLCBfX3dlYnBhY2tfZXhwb3J0c19fLCBfX3dlYnBhY2tfcmVxdWlyZV9fKSB7XG5cblwidXNlIHN0cmljdFwiO1xuXG4vLyBFWFBPUlRTXG5fX3dlYnBhY2tfcmVxdWlyZV9fLmQoX193ZWJwYWNrX2V4cG9ydHNfXywge1xuICBcImRlZmF1bHRcIjogZnVuY3Rpb24oKSB7IHJldHVybiAvKiBiaW5kaW5nICovIGNsaXBib2FyZDsgfVxufSk7XG5cbi8vIEVYVEVSTkFMIE1PRFVMRTogLi9ub2RlX21vZHVsZXMvdGlueS1lbWl0dGVyL2luZGV4LmpzXG52YXIgdGlueV9lbWl0dGVyID0gX193ZWJwYWNrX3JlcXVpcmVfXygyNzkpO1xudmFyIHRpbnlfZW1pdHRlcl9kZWZhdWx0ID0gLyojX19QVVJFX18qL19fd2VicGFja19yZXF1aXJlX18ubih0aW55X2VtaXR0ZXIpO1xuLy8gRVhURVJOQUwgTU9EVUxFOiAuL25vZGVfbW9kdWxlcy9nb29kLWxpc3RlbmVyL3NyYy9saXN0ZW4uanNcbnZhciBsaXN0ZW4gPSBfX3dlYnBhY2tfcmVxdWlyZV9fKDM3MCk7XG52YXIgbGlzdGVuX2RlZmF1bHQgPSAvKiNfX1BVUkVfXyovX193ZWJwYWNrX3JlcXVpcmVfXy5uKGxpc3Rlbik7XG4vLyBFWFRFUk5BTCBNT0RVTEU6IC4vbm9kZV9tb2R1bGVzL3NlbGVjdC9zcmMvc2VsZWN0LmpzXG52YXIgc3JjX3NlbGVjdCA9IF9fd2VicGFja19yZXF1aXJlX18oODE3KTtcbnZhciBzZWxlY3RfZGVmYXVsdCA9IC8qI19fUFVSRV9fKi9fX3dlYnBhY2tfcmVxdWlyZV9fLm4oc3JjX3NlbGVjdCk7XG47Ly8gQ09OQ0FURU5BVEVEIE1PRFVMRTogLi9zcmMvY29tbW9uL2NvbW1hbmQuanNcbi8qKlxuICogRXhlY3V0ZXMgYSBnaXZlbiBvcGVyYXRpb24gdHlwZS5cbiAqIEBwYXJhbSB7U3RyaW5nfSB0eXBlXG4gKiBAcmV0dXJuIHtCb29sZWFufVxuICovXG5mdW5jdGlvbiBjb21tYW5kKHR5cGUpIHtcbiAgdHJ5IHtcbiAgICByZXR1cm4gZG9jdW1lbnQuZXhlY0NvbW1hbmQodHlwZSk7XG4gIH0gY2F0Y2ggKGVycikge1xuICAgIHJldHVybiBmYWxzZTtcbiAgfVxufVxuOy8vIENPTkNBVEVOQVRFRCBNT0RVTEU6IC4vc3JjL2FjdGlvbnMvY3V0LmpzXG5cblxuLyoqXG4gKiBDdXQgYWN0aW9uIHdyYXBwZXIuXG4gKiBAcGFyYW0ge1N0cmluZ3xIVE1MRWxlbWVudH0gdGFyZ2V0XG4gKiBAcmV0dXJuIHtTdHJpbmd9XG4gKi9cblxudmFyIENsaXBib2FyZEFjdGlvbkN1dCA9IGZ1bmN0aW9uIENsaXBib2FyZEFjdGlvbkN1dCh0YXJnZXQpIHtcbiAgdmFyIHNlbGVjdGVkVGV4dCA9IHNlbGVjdF9kZWZhdWx0KCkodGFyZ2V0KTtcbiAgY29tbWFuZCgnY3V0Jyk7XG4gIHJldHVybiBzZWxlY3RlZFRleHQ7XG59O1xuXG4vKiBoYXJtb255IGRlZmF1bHQgZXhwb3J0ICovIHZhciBhY3Rpb25zX2N1dCA9IChDbGlwYm9hcmRBY3Rpb25DdXQpO1xuOy8vIENPTkNBVEVOQVRFRCBNT0RVTEU6IC4vc3JjL2NvbW1vbi9jcmVhdGUtZmFrZS1lbGVtZW50LmpzXG4vKipcbiAqIENyZWF0ZXMgYSBmYWtlIHRleHRhcmVhIGVsZW1lbnQgd2l0aCBhIHZhbHVlLlxuICogQHBhcmFtIHtTdHJpbmd9IHZhbHVlXG4gKiBAcmV0dXJuIHtIVE1MRWxlbWVudH1cbiAqL1xuZnVuY3Rpb24gY3JlYXRlRmFrZUVsZW1lbnQodmFsdWUpIHtcbiAgdmFyIGlzUlRMID0gZG9jdW1lbnQuZG9jdW1lbnRFbGVtZW50LmdldEF0dHJpYnV0ZSgnZGlyJykgPT09ICdydGwnO1xuICB2YXIgZmFrZUVsZW1lbnQgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCd0ZXh0YXJlYScpOyAvLyBQcmV2ZW50IHpvb21pbmcgb24gaU9TXG5cbiAgZmFrZUVsZW1lbnQuc3R5bGUuZm9udFNpemUgPSAnMTJwdCc7IC8vIFJlc2V0IGJveCBtb2RlbFxuXG4gIGZha2VFbGVtZW50LnN0eWxlLmJvcmRlciA9ICcwJztcbiAgZmFrZUVsZW1lbnQuc3R5bGUucGFkZGluZyA9ICcwJztcbiAgZmFrZUVsZW1lbnQuc3R5bGUubWFyZ2luID0gJzAnOyAvLyBNb3ZlIGVsZW1lbnQgb3V0IG9mIHNjcmVlbiBob3Jpem9udGFsbHlcblxuICBmYWtlRWxlbWVudC5zdHlsZS5wb3NpdGlvbiA9ICdhYnNvbHV0ZSc7XG4gIGZha2VFbGVtZW50LnN0eWxlW2lzUlRMID8gJ3JpZ2h0JyA6ICdsZWZ0J10gPSAnLTk5OTlweCc7IC8vIE1vdmUgZWxlbWVudCB0byB0aGUgc2FtZSBwb3NpdGlvbiB2ZXJ0aWNhbGx5XG5cbiAgdmFyIHlQb3NpdGlvbiA9IHdpbmRvdy5wYWdlWU9mZnNldCB8fCBkb2N1bWVudC5kb2N1bWVudEVsZW1lbnQuc2Nyb2xsVG9wO1xuICBmYWtlRWxlbWVudC5zdHlsZS50b3AgPSBcIlwiLmNvbmNhdCh5UG9zaXRpb24sIFwicHhcIik7XG4gIGZha2VFbGVtZW50LnNldEF0dHJpYnV0ZSgncmVhZG9ubHknLCAnJyk7XG4gIGZha2VFbGVtZW50LnZhbHVlID0gdmFsdWU7XG4gIHJldHVybiBmYWtlRWxlbWVudDtcbn1cbjsvLyBDT05DQVRFTkFURUQgTU9EVUxFOiAuL3NyYy9hY3Rpb25zL2NvcHkuanNcblxuXG5cbi8qKlxuICogQ3JlYXRlIGZha2UgY29weSBhY3Rpb24gd3JhcHBlciB1c2luZyBhIGZha2UgZWxlbWVudC5cbiAqIEBwYXJhbSB7U3RyaW5nfSB0YXJnZXRcbiAqIEBwYXJhbSB7T2JqZWN0fSBvcHRpb25zXG4gKiBAcmV0dXJuIHtTdHJpbmd9XG4gKi9cblxudmFyIGZha2VDb3B5QWN0aW9uID0gZnVuY3Rpb24gZmFrZUNvcHlBY3Rpb24odmFsdWUsIG9wdGlvbnMpIHtcbiAgdmFyIGZha2VFbGVtZW50ID0gY3JlYXRlRmFrZUVsZW1lbnQodmFsdWUpO1xuICBvcHRpb25zLmNvbnRhaW5lci5hcHBlbmRDaGlsZChmYWtlRWxlbWVudCk7XG4gIHZhciBzZWxlY3RlZFRleHQgPSBzZWxlY3RfZGVmYXVsdCgpKGZha2VFbGVtZW50KTtcbiAgY29tbWFuZCgnY29weScpO1xuICBmYWtlRWxlbWVudC5yZW1vdmUoKTtcbiAgcmV0dXJuIHNlbGVjdGVkVGV4dDtcbn07XG4vKipcbiAqIENvcHkgYWN0aW9uIHdyYXBwZXIuXG4gKiBAcGFyYW0ge1N0cmluZ3xIVE1MRWxlbWVudH0gdGFyZ2V0XG4gKiBAcGFyYW0ge09iamVjdH0gb3B0aW9uc1xuICogQHJldHVybiB7U3RyaW5nfVxuICovXG5cblxudmFyIENsaXBib2FyZEFjdGlvbkNvcHkgPSBmdW5jdGlvbiBDbGlwYm9hcmRBY3Rpb25Db3B5KHRhcmdldCkge1xuICB2YXIgb3B0aW9ucyA9IGFyZ3VtZW50cy5sZW5ndGggPiAxICYmIGFyZ3VtZW50c1sxXSAhPT0gdW5kZWZpbmVkID8gYXJndW1lbnRzWzFdIDoge1xuICAgIGNvbnRhaW5lcjogZG9jdW1lbnQuYm9keVxuICB9O1xuICB2YXIgc2VsZWN0ZWRUZXh0ID0gJyc7XG5cbiAgaWYgKHR5cGVvZiB0YXJnZXQgPT09ICdzdHJpbmcnKSB7XG4gICAgc2VsZWN0ZWRUZXh0ID0gZmFrZUNvcHlBY3Rpb24odGFyZ2V0LCBvcHRpb25zKTtcbiAgfSBlbHNlIGlmICh0YXJnZXQgaW5zdGFuY2VvZiBIVE1MSW5wdXRFbGVtZW50ICYmICFbJ3RleHQnLCAnc2VhcmNoJywgJ3VybCcsICd0ZWwnLCAncGFzc3dvcmQnXS5pbmNsdWRlcyh0YXJnZXQgPT09IG51bGwgfHwgdGFyZ2V0ID09PSB2b2lkIDAgPyB2b2lkIDAgOiB0YXJnZXQudHlwZSkpIHtcbiAgICAvLyBJZiBpbnB1dCB0eXBlIGRvZXNuJ3Qgc3VwcG9ydCBgc2V0U2VsZWN0aW9uUmFuZ2VgLiBTaW11bGF0ZSBpdC4gaHR0cHM6Ly9kZXZlbG9wZXIubW96aWxsYS5vcmcvZW4tVVMvZG9jcy9XZWIvQVBJL0hUTUxJbnB1dEVsZW1lbnQvc2V0U2VsZWN0aW9uUmFuZ2VcbiAgICBzZWxlY3RlZFRleHQgPSBmYWtlQ29weUFjdGlvbih0YXJnZXQudmFsdWUsIG9wdGlvbnMpO1xuICB9IGVsc2Uge1xuICAgIHNlbGVjdGVkVGV4dCA9IHNlbGVjdF9kZWZhdWx0KCkodGFyZ2V0KTtcbiAgICBjb21tYW5kKCdjb3B5Jyk7XG4gIH1cblxuICByZXR1cm4gc2VsZWN0ZWRUZXh0O1xufTtcblxuLyogaGFybW9ueSBkZWZhdWx0IGV4cG9ydCAqLyB2YXIgYWN0aW9uc19jb3B5ID0gKENsaXBib2FyZEFjdGlvbkNvcHkpO1xuOy8vIENPTkNBVEVOQVRFRCBNT0RVTEU6IC4vc3JjL2FjdGlvbnMvZGVmYXVsdC5qc1xuZnVuY3Rpb24gX3R5cGVvZihvYmopIHsgXCJAYmFiZWwvaGVscGVycyAtIHR5cGVvZlwiOyBpZiAodHlwZW9mIFN5bWJvbCA9PT0gXCJmdW5jdGlvblwiICYmIHR5cGVvZiBTeW1ib2wuaXRlcmF0b3IgPT09IFwic3ltYm9sXCIpIHsgX3R5cGVvZiA9IGZ1bmN0aW9uIF90eXBlb2Yob2JqKSB7IHJldHVybiB0eXBlb2Ygb2JqOyB9OyB9IGVsc2UgeyBfdHlwZW9mID0gZnVuY3Rpb24gX3R5cGVvZihvYmopIHsgcmV0dXJuIG9iaiAmJiB0eXBlb2YgU3ltYm9sID09PSBcImZ1bmN0aW9uXCIgJiYgb2JqLmNvbnN0cnVjdG9yID09PSBTeW1ib2wgJiYgb2JqICE9PSBTeW1ib2wucHJvdG90eXBlID8gXCJzeW1ib2xcIiA6IHR5cGVvZiBvYmo7IH07IH0gcmV0dXJuIF90eXBlb2Yob2JqKTsgfVxuXG5cblxuLyoqXG4gKiBJbm5lciBmdW5jdGlvbiB3aGljaCBwZXJmb3JtcyBzZWxlY3Rpb24gZnJvbSBlaXRoZXIgYHRleHRgIG9yIGB0YXJnZXRgXG4gKiBwcm9wZXJ0aWVzIGFuZCB0aGVuIGV4ZWN1dGVzIGNvcHkgb3IgY3V0IG9wZXJhdGlvbnMuXG4gKiBAcGFyYW0ge09iamVjdH0gb3B0aW9uc1xuICovXG5cbnZhciBDbGlwYm9hcmRBY3Rpb25EZWZhdWx0ID0gZnVuY3Rpb24gQ2xpcGJvYXJkQWN0aW9uRGVmYXVsdCgpIHtcbiAgdmFyIG9wdGlvbnMgPSBhcmd1bWVudHMubGVuZ3RoID4gMCAmJiBhcmd1bWVudHNbMF0gIT09IHVuZGVmaW5lZCA/IGFyZ3VtZW50c1swXSA6IHt9O1xuICAvLyBEZWZpbmVzIGJhc2UgcHJvcGVydGllcyBwYXNzZWQgZnJvbSBjb25zdHJ1Y3Rvci5cbiAgdmFyIF9vcHRpb25zJGFjdGlvbiA9IG9wdGlvbnMuYWN0aW9uLFxuICAgICAgYWN0aW9uID0gX29wdGlvbnMkYWN0aW9uID09PSB2b2lkIDAgPyAnY29weScgOiBfb3B0aW9ucyRhY3Rpb24sXG4gICAgICBjb250YWluZXIgPSBvcHRpb25zLmNvbnRhaW5lcixcbiAgICAgIHRhcmdldCA9IG9wdGlvbnMudGFyZ2V0LFxuICAgICAgdGV4dCA9IG9wdGlvbnMudGV4dDsgLy8gU2V0cyB0aGUgYGFjdGlvbmAgdG8gYmUgcGVyZm9ybWVkIHdoaWNoIGNhbiBiZSBlaXRoZXIgJ2NvcHknIG9yICdjdXQnLlxuXG4gIGlmIChhY3Rpb24gIT09ICdjb3B5JyAmJiBhY3Rpb24gIT09ICdjdXQnKSB7XG4gICAgdGhyb3cgbmV3IEVycm9yKCdJbnZhbGlkIFwiYWN0aW9uXCIgdmFsdWUsIHVzZSBlaXRoZXIgXCJjb3B5XCIgb3IgXCJjdXRcIicpO1xuICB9IC8vIFNldHMgdGhlIGB0YXJnZXRgIHByb3BlcnR5IHVzaW5nIGFuIGVsZW1lbnQgdGhhdCB3aWxsIGJlIGhhdmUgaXRzIGNvbnRlbnQgY29waWVkLlxuXG5cbiAgaWYgKHRhcmdldCAhPT0gdW5kZWZpbmVkKSB7XG4gICAgaWYgKHRhcmdldCAmJiBfdHlwZW9mKHRhcmdldCkgPT09ICdvYmplY3QnICYmIHRhcmdldC5ub2RlVHlwZSA9PT0gMSkge1xuICAgICAgaWYgKGFjdGlvbiA9PT0gJ2NvcHknICYmIHRhcmdldC5oYXNBdHRyaWJ1dGUoJ2Rpc2FibGVkJykpIHtcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKCdJbnZhbGlkIFwidGFyZ2V0XCIgYXR0cmlidXRlLiBQbGVhc2UgdXNlIFwicmVhZG9ubHlcIiBpbnN0ZWFkIG9mIFwiZGlzYWJsZWRcIiBhdHRyaWJ1dGUnKTtcbiAgICAgIH1cblxuICAgICAgaWYgKGFjdGlvbiA9PT0gJ2N1dCcgJiYgKHRhcmdldC5oYXNBdHRyaWJ1dGUoJ3JlYWRvbmx5JykgfHwgdGFyZ2V0Lmhhc0F0dHJpYnV0ZSgnZGlzYWJsZWQnKSkpIHtcbiAgICAgICAgdGhyb3cgbmV3IEVycm9yKCdJbnZhbGlkIFwidGFyZ2V0XCIgYXR0cmlidXRlLiBZb3UgY2FuXFwndCBjdXQgdGV4dCBmcm9tIGVsZW1lbnRzIHdpdGggXCJyZWFkb25seVwiIG9yIFwiZGlzYWJsZWRcIiBhdHRyaWJ1dGVzJyk7XG4gICAgICB9XG4gICAgfSBlbHNlIHtcbiAgICAgIHRocm93IG5ldyBFcnJvcignSW52YWxpZCBcInRhcmdldFwiIHZhbHVlLCB1c2UgYSB2YWxpZCBFbGVtZW50Jyk7XG4gICAgfVxuICB9IC8vIERlZmluZSBzZWxlY3Rpb24gc3RyYXRlZ3kgYmFzZWQgb24gYHRleHRgIHByb3BlcnR5LlxuXG5cbiAgaWYgKHRleHQpIHtcbiAgICByZXR1cm4gYWN0aW9uc19jb3B5KHRleHQsIHtcbiAgICAgIGNvbnRhaW5lcjogY29udGFpbmVyXG4gICAgfSk7XG4gIH0gLy8gRGVmaW5lcyB3aGljaCBzZWxlY3Rpb24gc3RyYXRlZ3kgYmFzZWQgb24gYHRhcmdldGAgcHJvcGVydHkuXG5cblxuICBpZiAodGFyZ2V0KSB7XG4gICAgcmV0dXJuIGFjdGlvbiA9PT0gJ2N1dCcgPyBhY3Rpb25zX2N1dCh0YXJnZXQpIDogYWN0aW9uc19jb3B5KHRhcmdldCwge1xuICAgICAgY29udGFpbmVyOiBjb250YWluZXJcbiAgICB9KTtcbiAgfVxufTtcblxuLyogaGFybW9ueSBkZWZhdWx0IGV4cG9ydCAqLyB2YXIgYWN0aW9uc19kZWZhdWx0ID0gKENsaXBib2FyZEFjdGlvbkRlZmF1bHQpO1xuOy8vIENPTkNBVEVOQVRFRCBNT0RVTEU6IC4vc3JjL2NsaXBib2FyZC5qc1xuZnVuY3Rpb24gY2xpcGJvYXJkX3R5cGVvZihvYmopIHsgXCJAYmFiZWwvaGVscGVycyAtIHR5cGVvZlwiOyBpZiAodHlwZW9mIFN5bWJvbCA9PT0gXCJmdW5jdGlvblwiICYmIHR5cGVvZiBTeW1ib2wuaXRlcmF0b3IgPT09IFwic3ltYm9sXCIpIHsgY2xpcGJvYXJkX3R5cGVvZiA9IGZ1bmN0aW9uIF90eXBlb2Yob2JqKSB7IHJldHVybiB0eXBlb2Ygb2JqOyB9OyB9IGVsc2UgeyBjbGlwYm9hcmRfdHlwZW9mID0gZnVuY3Rpb24gX3R5cGVvZihvYmopIHsgcmV0dXJuIG9iaiAmJiB0eXBlb2YgU3ltYm9sID09PSBcImZ1bmN0aW9uXCIgJiYgb2JqLmNvbnN0cnVjdG9yID09PSBTeW1ib2wgJiYgb2JqICE9PSBTeW1ib2wucHJvdG90eXBlID8gXCJzeW1ib2xcIiA6IHR5cGVvZiBvYmo7IH07IH0gcmV0dXJuIGNsaXBib2FyZF90eXBlb2Yob2JqKTsgfVxuXG5mdW5jdGlvbiBfY2xhc3NDYWxsQ2hlY2soaW5zdGFuY2UsIENvbnN0cnVjdG9yKSB7IGlmICghKGluc3RhbmNlIGluc3RhbmNlb2YgQ29uc3RydWN0b3IpKSB7IHRocm93IG5ldyBUeXBlRXJyb3IoXCJDYW5ub3QgY2FsbCBhIGNsYXNzIGFzIGEgZnVuY3Rpb25cIik7IH0gfVxuXG5mdW5jdGlvbiBfZGVmaW5lUHJvcGVydGllcyh0YXJnZXQsIHByb3BzKSB7IGZvciAodmFyIGkgPSAwOyBpIDwgcHJvcHMubGVuZ3RoOyBpKyspIHsgdmFyIGRlc2NyaXB0b3IgPSBwcm9wc1tpXTsgZGVzY3JpcHRvci5lbnVtZXJhYmxlID0gZGVzY3JpcHRvci5lbnVtZXJhYmxlIHx8IGZhbHNlOyBkZXNjcmlwdG9yLmNvbmZpZ3VyYWJsZSA9IHRydWU7IGlmIChcInZhbHVlXCIgaW4gZGVzY3JpcHRvcikgZGVzY3JpcHRvci53cml0YWJsZSA9IHRydWU7IE9iamVjdC5kZWZpbmVQcm9wZXJ0eSh0YXJnZXQsIGRlc2NyaXB0b3Iua2V5LCBkZXNjcmlwdG9yKTsgfSB9XG5cbmZ1bmN0aW9uIF9jcmVhdGVDbGFzcyhDb25zdHJ1Y3RvciwgcHJvdG9Qcm9wcywgc3RhdGljUHJvcHMpIHsgaWYgKHByb3RvUHJvcHMpIF9kZWZpbmVQcm9wZXJ0aWVzKENvbnN0cnVjdG9yLnByb3RvdHlwZSwgcHJvdG9Qcm9wcyk7IGlmIChzdGF0aWNQcm9wcykgX2RlZmluZVByb3BlcnRpZXMoQ29uc3RydWN0b3IsIHN0YXRpY1Byb3BzKTsgcmV0dXJuIENvbnN0cnVjdG9yOyB9XG5cbmZ1bmN0aW9uIF9pbmhlcml0cyhzdWJDbGFzcywgc3VwZXJDbGFzcykgeyBpZiAodHlwZW9mIHN1cGVyQ2xhc3MgIT09IFwiZnVuY3Rpb25cIiAmJiBzdXBlckNsYXNzICE9PSBudWxsKSB7IHRocm93IG5ldyBUeXBlRXJyb3IoXCJTdXBlciBleHByZXNzaW9uIG11c3QgZWl0aGVyIGJlIG51bGwgb3IgYSBmdW5jdGlvblwiKTsgfSBzdWJDbGFzcy5wcm90b3R5cGUgPSBPYmplY3QuY3JlYXRlKHN1cGVyQ2xhc3MgJiYgc3VwZXJDbGFzcy5wcm90b3R5cGUsIHsgY29uc3RydWN0b3I6IHsgdmFsdWU6IHN1YkNsYXNzLCB3cml0YWJsZTogdHJ1ZSwgY29uZmlndXJhYmxlOiB0cnVlIH0gfSk7IGlmIChzdXBlckNsYXNzKSBfc2V0UHJvdG90eXBlT2Yoc3ViQ2xhc3MsIHN1cGVyQ2xhc3MpOyB9XG5cbmZ1bmN0aW9uIF9zZXRQcm90b3R5cGVPZihvLCBwKSB7IF9zZXRQcm90b3R5cGVPZiA9IE9iamVjdC5zZXRQcm90b3R5cGVPZiB8fCBmdW5jdGlvbiBfc2V0UHJvdG90eXBlT2YobywgcCkgeyBvLl9fcHJvdG9fXyA9IHA7IHJldHVybiBvOyB9OyByZXR1cm4gX3NldFByb3RvdHlwZU9mKG8sIHApOyB9XG5cbmZ1bmN0aW9uIF9jcmVhdGVTdXBlcihEZXJpdmVkKSB7IHZhciBoYXNOYXRpdmVSZWZsZWN0Q29uc3RydWN0ID0gX2lzTmF0aXZlUmVmbGVjdENvbnN0cnVjdCgpOyByZXR1cm4gZnVuY3Rpb24gX2NyZWF0ZVN1cGVySW50ZXJuYWwoKSB7IHZhciBTdXBlciA9IF9nZXRQcm90b3R5cGVPZihEZXJpdmVkKSwgcmVzdWx0OyBpZiAoaGFzTmF0aXZlUmVmbGVjdENvbnN0cnVjdCkgeyB2YXIgTmV3VGFyZ2V0ID0gX2dldFByb3RvdHlwZU9mKHRoaXMpLmNvbnN0cnVjdG9yOyByZXN1bHQgPSBSZWZsZWN0LmNvbnN0cnVjdChTdXBlciwgYXJndW1lbnRzLCBOZXdUYXJnZXQpOyB9IGVsc2UgeyByZXN1bHQgPSBTdXBlci5hcHBseSh0aGlzLCBhcmd1bWVudHMpOyB9IHJldHVybiBfcG9zc2libGVDb25zdHJ1Y3RvclJldHVybih0aGlzLCByZXN1bHQpOyB9OyB9XG5cbmZ1bmN0aW9uIF9wb3NzaWJsZUNvbnN0cnVjdG9yUmV0dXJuKHNlbGYsIGNhbGwpIHsgaWYgKGNhbGwgJiYgKGNsaXBib2FyZF90eXBlb2YoY2FsbCkgPT09IFwib2JqZWN0XCIgfHwgdHlwZW9mIGNhbGwgPT09IFwiZnVuY3Rpb25cIikpIHsgcmV0dXJuIGNhbGw7IH0gcmV0dXJuIF9hc3NlcnRUaGlzSW5pdGlhbGl6ZWQoc2VsZik7IH1cblxuZnVuY3Rpb24gX2Fzc2VydFRoaXNJbml0aWFsaXplZChzZWxmKSB7IGlmIChzZWxmID09PSB2b2lkIDApIHsgdGhyb3cgbmV3IFJlZmVyZW5jZUVycm9yKFwidGhpcyBoYXNuJ3QgYmVlbiBpbml0aWFsaXNlZCAtIHN1cGVyKCkgaGFzbid0IGJlZW4gY2FsbGVkXCIpOyB9IHJldHVybiBzZWxmOyB9XG5cbmZ1bmN0aW9uIF9pc05hdGl2ZVJlZmxlY3RDb25zdHJ1Y3QoKSB7IGlmICh0eXBlb2YgUmVmbGVjdCA9PT0gXCJ1bmRlZmluZWRcIiB8fCAhUmVmbGVjdC5jb25zdHJ1Y3QpIHJldHVybiBmYWxzZTsgaWYgKFJlZmxlY3QuY29uc3RydWN0LnNoYW0pIHJldHVybiBmYWxzZTsgaWYgKHR5cGVvZiBQcm94eSA9PT0gXCJmdW5jdGlvblwiKSByZXR1cm4gdHJ1ZTsgdHJ5IHsgRGF0ZS5wcm90b3R5cGUudG9TdHJpbmcuY2FsbChSZWZsZWN0LmNvbnN0cnVjdChEYXRlLCBbXSwgZnVuY3Rpb24gKCkge30pKTsgcmV0dXJuIHRydWU7IH0gY2F0Y2ggKGUpIHsgcmV0dXJuIGZhbHNlOyB9IH1cblxuZnVuY3Rpb24gX2dldFByb3RvdHlwZU9mKG8pIHsgX2dldFByb3RvdHlwZU9mID0gT2JqZWN0LnNldFByb3RvdHlwZU9mID8gT2JqZWN0LmdldFByb3RvdHlwZU9mIDogZnVuY3Rpb24gX2dldFByb3RvdHlwZU9mKG8pIHsgcmV0dXJuIG8uX19wcm90b19fIHx8IE9iamVjdC5nZXRQcm90b3R5cGVPZihvKTsgfTsgcmV0dXJuIF9nZXRQcm90b3R5cGVPZihvKTsgfVxuXG5cblxuXG5cblxuLyoqXG4gKiBIZWxwZXIgZnVuY3Rpb24gdG8gcmV0cmlldmUgYXR0cmlidXRlIHZhbHVlLlxuICogQHBhcmFtIHtTdHJpbmd9IHN1ZmZpeFxuICogQHBhcmFtIHtFbGVtZW50fSBlbGVtZW50XG4gKi9cblxuZnVuY3Rpb24gZ2V0QXR0cmlidXRlVmFsdWUoc3VmZml4LCBlbGVtZW50KSB7XG4gIHZhciBhdHRyaWJ1dGUgPSBcImRhdGEtY2xpcGJvYXJkLVwiLmNvbmNhdChzdWZmaXgpO1xuXG4gIGlmICghZWxlbWVudC5oYXNBdHRyaWJ1dGUoYXR0cmlidXRlKSkge1xuICAgIHJldHVybjtcbiAgfVxuXG4gIHJldHVybiBlbGVtZW50LmdldEF0dHJpYnV0ZShhdHRyaWJ1dGUpO1xufVxuLyoqXG4gKiBCYXNlIGNsYXNzIHdoaWNoIHRha2VzIG9uZSBvciBtb3JlIGVsZW1lbnRzLCBhZGRzIGV2ZW50IGxpc3RlbmVycyB0byB0aGVtLFxuICogYW5kIGluc3RhbnRpYXRlcyBhIG5ldyBgQ2xpcGJvYXJkQWN0aW9uYCBvbiBlYWNoIGNsaWNrLlxuICovXG5cblxudmFyIENsaXBib2FyZCA9IC8qI19fUFVSRV9fKi9mdW5jdGlvbiAoX0VtaXR0ZXIpIHtcbiAgX2luaGVyaXRzKENsaXBib2FyZCwgX0VtaXR0ZXIpO1xuXG4gIHZhciBfc3VwZXIgPSBfY3JlYXRlU3VwZXIoQ2xpcGJvYXJkKTtcblxuICAvKipcbiAgICogQHBhcmFtIHtTdHJpbmd8SFRNTEVsZW1lbnR8SFRNTENvbGxlY3Rpb258Tm9kZUxpc3R9IHRyaWdnZXJcbiAgICogQHBhcmFtIHtPYmplY3R9IG9wdGlvbnNcbiAgICovXG4gIGZ1bmN0aW9uIENsaXBib2FyZCh0cmlnZ2VyLCBvcHRpb25zKSB7XG4gICAgdmFyIF90aGlzO1xuXG4gICAgX2NsYXNzQ2FsbENoZWNrKHRoaXMsIENsaXBib2FyZCk7XG5cbiAgICBfdGhpcyA9IF9zdXBlci5jYWxsKHRoaXMpO1xuXG4gICAgX3RoaXMucmVzb2x2ZU9wdGlvbnMob3B0aW9ucyk7XG5cbiAgICBfdGhpcy5saXN0ZW5DbGljayh0cmlnZ2VyKTtcblxuICAgIHJldHVybiBfdGhpcztcbiAgfVxuICAvKipcbiAgICogRGVmaW5lcyBpZiBhdHRyaWJ1dGVzIHdvdWxkIGJlIHJlc29sdmVkIHVzaW5nIGludGVybmFsIHNldHRlciBmdW5jdGlvbnNcbiAgICogb3IgY3VzdG9tIGZ1bmN0aW9ucyB0aGF0IHdlcmUgcGFzc2VkIGluIHRoZSBjb25zdHJ1Y3Rvci5cbiAgICogQHBhcmFtIHtPYmplY3R9IG9wdGlvbnNcbiAgICovXG5cblxuICBfY3JlYXRlQ2xhc3MoQ2xpcGJvYXJkLCBbe1xuICAgIGtleTogXCJyZXNvbHZlT3B0aW9uc1wiLFxuICAgIHZhbHVlOiBmdW5jdGlvbiByZXNvbHZlT3B0aW9ucygpIHtcbiAgICAgIHZhciBvcHRpb25zID0gYXJndW1lbnRzLmxlbmd0aCA+IDAgJiYgYXJndW1lbnRzWzBdICE9PSB1bmRlZmluZWQgPyBhcmd1bWVudHNbMF0gOiB7fTtcbiAgICAgIHRoaXMuYWN0aW9uID0gdHlwZW9mIG9wdGlvbnMuYWN0aW9uID09PSAnZnVuY3Rpb24nID8gb3B0aW9ucy5hY3Rpb24gOiB0aGlzLmRlZmF1bHRBY3Rpb247XG4gICAgICB0aGlzLnRhcmdldCA9IHR5cGVvZiBvcHRpb25zLnRhcmdldCA9PT0gJ2Z1bmN0aW9uJyA/IG9wdGlvbnMudGFyZ2V0IDogdGhpcy5kZWZhdWx0VGFyZ2V0O1xuICAgICAgdGhpcy50ZXh0ID0gdHlwZW9mIG9wdGlvbnMudGV4dCA9PT0gJ2Z1bmN0aW9uJyA/IG9wdGlvbnMudGV4dCA6IHRoaXMuZGVmYXVsdFRleHQ7XG4gICAgICB0aGlzLmNvbnRhaW5lciA9IGNsaXBib2FyZF90eXBlb2Yob3B0aW9ucy5jb250YWluZXIpID09PSAnb2JqZWN0JyA/IG9wdGlvbnMuY29udGFpbmVyIDogZG9jdW1lbnQuYm9keTtcbiAgICB9XG4gICAgLyoqXG4gICAgICogQWRkcyBhIGNsaWNrIGV2ZW50IGxpc3RlbmVyIHRvIHRoZSBwYXNzZWQgdHJpZ2dlci5cbiAgICAgKiBAcGFyYW0ge1N0cmluZ3xIVE1MRWxlbWVudHxIVE1MQ29sbGVjdGlvbnxOb2RlTGlzdH0gdHJpZ2dlclxuICAgICAqL1xuXG4gIH0sIHtcbiAgICBrZXk6IFwibGlzdGVuQ2xpY2tcIixcbiAgICB2YWx1ZTogZnVuY3Rpb24gbGlzdGVuQ2xpY2sodHJpZ2dlcikge1xuICAgICAgdmFyIF90aGlzMiA9IHRoaXM7XG5cbiAgICAgIHRoaXMubGlzdGVuZXIgPSBsaXN0ZW5fZGVmYXVsdCgpKHRyaWdnZXIsICdjbGljaycsIGZ1bmN0aW9uIChlKSB7XG4gICAgICAgIHJldHVybiBfdGhpczIub25DbGljayhlKTtcbiAgICAgIH0pO1xuICAgIH1cbiAgICAvKipcbiAgICAgKiBEZWZpbmVzIGEgbmV3IGBDbGlwYm9hcmRBY3Rpb25gIG9uIGVhY2ggY2xpY2sgZXZlbnQuXG4gICAgICogQHBhcmFtIHtFdmVudH0gZVxuICAgICAqL1xuXG4gIH0sIHtcbiAgICBrZXk6IFwib25DbGlja1wiLFxuICAgIHZhbHVlOiBmdW5jdGlvbiBvbkNsaWNrKGUpIHtcbiAgICAgIHZhciB0cmlnZ2VyID0gZS5kZWxlZ2F0ZVRhcmdldCB8fCBlLmN1cnJlbnRUYXJnZXQ7XG4gICAgICB2YXIgYWN0aW9uID0gdGhpcy5hY3Rpb24odHJpZ2dlcikgfHwgJ2NvcHknO1xuICAgICAgdmFyIHRleHQgPSBhY3Rpb25zX2RlZmF1bHQoe1xuICAgICAgICBhY3Rpb246IGFjdGlvbixcbiAgICAgICAgY29udGFpbmVyOiB0aGlzLmNvbnRhaW5lcixcbiAgICAgICAgdGFyZ2V0OiB0aGlzLnRhcmdldCh0cmlnZ2VyKSxcbiAgICAgICAgdGV4dDogdGhpcy50ZXh0KHRyaWdnZXIpXG4gICAgICB9KTsgLy8gRmlyZXMgYW4gZXZlbnQgYmFzZWQgb24gdGhlIGNvcHkgb3BlcmF0aW9uIHJlc3VsdC5cblxuICAgICAgdGhpcy5lbWl0KHRleHQgPyAnc3VjY2VzcycgOiAnZXJyb3InLCB7XG4gICAgICAgIGFjdGlvbjogYWN0aW9uLFxuICAgICAgICB0ZXh0OiB0ZXh0LFxuICAgICAgICB0cmlnZ2VyOiB0cmlnZ2VyLFxuICAgICAgICBjbGVhclNlbGVjdGlvbjogZnVuY3Rpb24gY2xlYXJTZWxlY3Rpb24oKSB7XG4gICAgICAgICAgaWYgKHRyaWdnZXIpIHtcbiAgICAgICAgICAgIHRyaWdnZXIuZm9jdXMoKTtcbiAgICAgICAgICB9XG5cbiAgICAgICAgICB3aW5kb3cuZ2V0U2VsZWN0aW9uKCkucmVtb3ZlQWxsUmFuZ2VzKCk7XG4gICAgICAgIH1cbiAgICAgIH0pO1xuICAgIH1cbiAgICAvKipcbiAgICAgKiBEZWZhdWx0IGBhY3Rpb25gIGxvb2t1cCBmdW5jdGlvbi5cbiAgICAgKiBAcGFyYW0ge0VsZW1lbnR9IHRyaWdnZXJcbiAgICAgKi9cblxuICB9LCB7XG4gICAga2V5OiBcImRlZmF1bHRBY3Rpb25cIixcbiAgICB2YWx1ZTogZnVuY3Rpb24gZGVmYXVsdEFjdGlvbih0cmlnZ2VyKSB7XG4gICAgICByZXR1cm4gZ2V0QXR0cmlidXRlVmFsdWUoJ2FjdGlvbicsIHRyaWdnZXIpO1xuICAgIH1cbiAgICAvKipcbiAgICAgKiBEZWZhdWx0IGB0YXJnZXRgIGxvb2t1cCBmdW5jdGlvbi5cbiAgICAgKiBAcGFyYW0ge0VsZW1lbnR9IHRyaWdnZXJcbiAgICAgKi9cblxuICB9LCB7XG4gICAga2V5OiBcImRlZmF1bHRUYXJnZXRcIixcbiAgICB2YWx1ZTogZnVuY3Rpb24gZGVmYXVsdFRhcmdldCh0cmlnZ2VyKSB7XG4gICAgICB2YXIgc2VsZWN0b3IgPSBnZXRBdHRyaWJ1dGVWYWx1ZSgndGFyZ2V0JywgdHJpZ2dlcik7XG5cbiAgICAgIGlmIChzZWxlY3Rvcikge1xuICAgICAgICByZXR1cm4gZG9jdW1lbnQucXVlcnlTZWxlY3RvcihzZWxlY3Rvcik7XG4gICAgICB9XG4gICAgfVxuICAgIC8qKlxuICAgICAqIEFsbG93IGZpcmUgcHJvZ3JhbW1hdGljYWxseSBhIGNvcHkgYWN0aW9uXG4gICAgICogQHBhcmFtIHtTdHJpbmd8SFRNTEVsZW1lbnR9IHRhcmdldFxuICAgICAqIEBwYXJhbSB7T2JqZWN0fSBvcHRpb25zXG4gICAgICogQHJldHVybnMgVGV4dCBjb3BpZWQuXG4gICAgICovXG5cbiAgfSwge1xuICAgIGtleTogXCJkZWZhdWx0VGV4dFwiLFxuXG4gICAgLyoqXG4gICAgICogRGVmYXVsdCBgdGV4dGAgbG9va3VwIGZ1bmN0aW9uLlxuICAgICAqIEBwYXJhbSB7RWxlbWVudH0gdHJpZ2dlclxuICAgICAqL1xuICAgIHZhbHVlOiBmdW5jdGlvbiBkZWZhdWx0VGV4dCh0cmlnZ2VyKSB7XG4gICAgICByZXR1cm4gZ2V0QXR0cmlidXRlVmFsdWUoJ3RleHQnLCB0cmlnZ2VyKTtcbiAgICB9XG4gICAgLyoqXG4gICAgICogRGVzdHJveSBsaWZlY3ljbGUuXG4gICAgICovXG5cbiAgfSwge1xuICAgIGtleTogXCJkZXN0cm95XCIsXG4gICAgdmFsdWU6IGZ1bmN0aW9uIGRlc3Ryb3koKSB7XG4gICAgICB0aGlzLmxpc3RlbmVyLmRlc3Ryb3koKTtcbiAgICB9XG4gIH1dLCBbe1xuICAgIGtleTogXCJjb3B5XCIsXG4gICAgdmFsdWU6IGZ1bmN0aW9uIGNvcHkodGFyZ2V0KSB7XG4gICAgICB2YXIgb3B0aW9ucyA9IGFyZ3VtZW50cy5sZW5ndGggPiAxICYmIGFyZ3VtZW50c1sxXSAhPT0gdW5kZWZpbmVkID8gYXJndW1lbnRzWzFdIDoge1xuICAgICAgICBjb250YWluZXI6IGRvY3VtZW50LmJvZHlcbiAgICAgIH07XG4gICAgICByZXR1cm4gYWN0aW9uc19jb3B5KHRhcmdldCwgb3B0aW9ucyk7XG4gICAgfVxuICAgIC8qKlxuICAgICAqIEFsbG93IGZpcmUgcHJvZ3JhbW1hdGljYWxseSBhIGN1dCBhY3Rpb25cbiAgICAgKiBAcGFyYW0ge1N0cmluZ3xIVE1MRWxlbWVudH0gdGFyZ2V0XG4gICAgICogQHJldHVybnMgVGV4dCBjdXR0ZWQuXG4gICAgICovXG5cbiAgfSwge1xuICAgIGtleTogXCJjdXRcIixcbiAgICB2YWx1ZTogZnVuY3Rpb24gY3V0KHRhcmdldCkge1xuICAgICAgcmV0dXJuIGFjdGlvbnNfY3V0KHRhcmdldCk7XG4gICAgfVxuICAgIC8qKlxuICAgICAqIFJldHVybnMgdGhlIHN1cHBvcnQgb2YgdGhlIGdpdmVuIGFjdGlvbiwgb3IgYWxsIGFjdGlvbnMgaWYgbm8gYWN0aW9uIGlzXG4gICAgICogZ2l2ZW4uXG4gICAgICogQHBhcmFtIHtTdHJpbmd9IFthY3Rpb25dXG4gICAgICovXG5cbiAgfSwge1xuICAgIGtleTogXCJpc1N1cHBvcnRlZFwiLFxuICAgIHZhbHVlOiBmdW5jdGlvbiBpc1N1cHBvcnRlZCgpIHtcbiAgICAgIHZhciBhY3Rpb24gPSBhcmd1bWVudHMubGVuZ3RoID4gMCAmJiBhcmd1bWVudHNbMF0gIT09IHVuZGVmaW5lZCA/IGFyZ3VtZW50c1swXSA6IFsnY29weScsICdjdXQnXTtcbiAgICAgIHZhciBhY3Rpb25zID0gdHlwZW9mIGFjdGlvbiA9PT0gJ3N0cmluZycgPyBbYWN0aW9uXSA6IGFjdGlvbjtcbiAgICAgIHZhciBzdXBwb3J0ID0gISFkb2N1bWVudC5xdWVyeUNvbW1hbmRTdXBwb3J0ZWQ7XG4gICAgICBhY3Rpb25zLmZvckVhY2goZnVuY3Rpb24gKGFjdGlvbikge1xuICAgICAgICBzdXBwb3J0ID0gc3VwcG9ydCAmJiAhIWRvY3VtZW50LnF1ZXJ5Q29tbWFuZFN1cHBvcnRlZChhY3Rpb24pO1xuICAgICAgfSk7XG4gICAgICByZXR1cm4gc3VwcG9ydDtcbiAgICB9XG4gIH1dKTtcblxuICByZXR1cm4gQ2xpcGJvYXJkO1xufSgodGlueV9lbWl0dGVyX2RlZmF1bHQoKSkpO1xuXG4vKiBoYXJtb255IGRlZmF1bHQgZXhwb3J0ICovIHZhciBjbGlwYm9hcmQgPSAoQ2xpcGJvYXJkKTtcblxuLyoqKi8gfSksXG5cbi8qKiovIDgyODpcbi8qKiovIChmdW5jdGlvbihtb2R1bGUpIHtcblxudmFyIERPQ1VNRU5UX05PREVfVFlQRSA9IDk7XG5cbi8qKlxuICogQSBwb2x5ZmlsbCBmb3IgRWxlbWVudC5tYXRjaGVzKClcbiAqL1xuaWYgKHR5cGVvZiBFbGVtZW50ICE9PSAndW5kZWZpbmVkJyAmJiAhRWxlbWVudC5wcm90b3R5cGUubWF0Y2hlcykge1xuICAgIHZhciBwcm90byA9IEVsZW1lbnQucHJvdG90eXBlO1xuXG4gICAgcHJvdG8ubWF0Y2hlcyA9IHByb3RvLm1hdGNoZXNTZWxlY3RvciB8fFxuICAgICAgICAgICAgICAgICAgICBwcm90by5tb3pNYXRjaGVzU2VsZWN0b3IgfHxcbiAgICAgICAgICAgICAgICAgICAgcHJvdG8ubXNNYXRjaGVzU2VsZWN0b3IgfHxcbiAgICAgICAgICAgICAgICAgICAgcHJvdG8ub01hdGNoZXNTZWxlY3RvciB8fFxuICAgICAgICAgICAgICAgICAgICBwcm90by53ZWJraXRNYXRjaGVzU2VsZWN0b3I7XG59XG5cbi8qKlxuICogRmluZHMgdGhlIGNsb3Nlc3QgcGFyZW50IHRoYXQgbWF0Y2hlcyBhIHNlbGVjdG9yLlxuICpcbiAqIEBwYXJhbSB7RWxlbWVudH0gZWxlbWVudFxuICogQHBhcmFtIHtTdHJpbmd9IHNlbGVjdG9yXG4gKiBAcmV0dXJuIHtGdW5jdGlvbn1cbiAqL1xuZnVuY3Rpb24gY2xvc2VzdCAoZWxlbWVudCwgc2VsZWN0b3IpIHtcbiAgICB3aGlsZSAoZWxlbWVudCAmJiBlbGVtZW50Lm5vZGVUeXBlICE9PSBET0NVTUVOVF9OT0RFX1RZUEUpIHtcbiAgICAgICAgaWYgKHR5cGVvZiBlbGVtZW50Lm1hdGNoZXMgPT09ICdmdW5jdGlvbicgJiZcbiAgICAgICAgICAgIGVsZW1lbnQubWF0Y2hlcyhzZWxlY3RvcikpIHtcbiAgICAgICAgICByZXR1cm4gZWxlbWVudDtcbiAgICAgICAgfVxuICAgICAgICBlbGVtZW50ID0gZWxlbWVudC5wYXJlbnROb2RlO1xuICAgIH1cbn1cblxubW9kdWxlLmV4cG9ydHMgPSBjbG9zZXN0O1xuXG5cbi8qKiovIH0pLFxuXG4vKioqLyA0Mzg6XG4vKioqLyAoZnVuY3Rpb24obW9kdWxlLCBfX3VudXNlZF93ZWJwYWNrX2V4cG9ydHMsIF9fd2VicGFja19yZXF1aXJlX18pIHtcblxudmFyIGNsb3Nlc3QgPSBfX3dlYnBhY2tfcmVxdWlyZV9fKDgyOCk7XG5cbi8qKlxuICogRGVsZWdhdGVzIGV2ZW50IHRvIGEgc2VsZWN0b3IuXG4gKlxuICogQHBhcmFtIHtFbGVtZW50fSBlbGVtZW50XG4gKiBAcGFyYW0ge1N0cmluZ30gc2VsZWN0b3JcbiAqIEBwYXJhbSB7U3RyaW5nfSB0eXBlXG4gKiBAcGFyYW0ge0Z1bmN0aW9ufSBjYWxsYmFja1xuICogQHBhcmFtIHtCb29sZWFufSB1c2VDYXB0dXJlXG4gKiBAcmV0dXJuIHtPYmplY3R9XG4gKi9cbmZ1bmN0aW9uIF9kZWxlZ2F0ZShlbGVtZW50LCBzZWxlY3RvciwgdHlwZSwgY2FsbGJhY2ssIHVzZUNhcHR1cmUpIHtcbiAgICB2YXIgbGlzdGVuZXJGbiA9IGxpc3RlbmVyLmFwcGx5KHRoaXMsIGFyZ3VtZW50cyk7XG5cbiAgICBlbGVtZW50LmFkZEV2ZW50TGlzdGVuZXIodHlwZSwgbGlzdGVuZXJGbiwgdXNlQ2FwdHVyZSk7XG5cbiAgICByZXR1cm4ge1xuICAgICAgICBkZXN0cm95OiBmdW5jdGlvbigpIHtcbiAgICAgICAgICAgIGVsZW1lbnQucmVtb3ZlRXZlbnRMaXN0ZW5lcih0eXBlLCBsaXN0ZW5lckZuLCB1c2VDYXB0dXJlKTtcbiAgICAgICAgfVxuICAgIH1cbn1cblxuLyoqXG4gKiBEZWxlZ2F0ZXMgZXZlbnQgdG8gYSBzZWxlY3Rvci5cbiAqXG4gKiBAcGFyYW0ge0VsZW1lbnR8U3RyaW5nfEFycmF5fSBbZWxlbWVudHNdXG4gKiBAcGFyYW0ge1N0cmluZ30gc2VsZWN0b3JcbiAqIEBwYXJhbSB7U3RyaW5nfSB0eXBlXG4gKiBAcGFyYW0ge0Z1bmN0aW9ufSBjYWxsYmFja1xuICogQHBhcmFtIHtCb29sZWFufSB1c2VDYXB0dXJlXG4gKiBAcmV0dXJuIHtPYmplY3R9XG4gKi9cbmZ1bmN0aW9uIGRlbGVnYXRlKGVsZW1lbnRzLCBzZWxlY3RvciwgdHlwZSwgY2FsbGJhY2ssIHVzZUNhcHR1cmUpIHtcbiAgICAvLyBIYW5kbGUgdGhlIHJlZ3VsYXIgRWxlbWVudCB1c2FnZVxuICAgIGlmICh0eXBlb2YgZWxlbWVudHMuYWRkRXZlbnRMaXN0ZW5lciA9PT0gJ2Z1bmN0aW9uJykge1xuICAgICAgICByZXR1cm4gX2RlbGVnYXRlLmFwcGx5KG51bGwsIGFyZ3VtZW50cyk7XG4gICAgfVxuXG4gICAgLy8gSGFuZGxlIEVsZW1lbnQtbGVzcyB1c2FnZSwgaXQgZGVmYXVsdHMgdG8gZ2xvYmFsIGRlbGVnYXRpb25cbiAgICBpZiAodHlwZW9mIHR5cGUgPT09ICdmdW5jdGlvbicpIHtcbiAgICAgICAgLy8gVXNlIGBkb2N1bWVudGAgYXMgdGhlIGZpcnN0IHBhcmFtZXRlciwgdGhlbiBhcHBseSBhcmd1bWVudHNcbiAgICAgICAgLy8gVGhpcyBpcyBhIHNob3J0IHdheSB0byAudW5zaGlmdCBgYXJndW1lbnRzYCB3aXRob3V0IHJ1bm5pbmcgaW50byBkZW9wdGltaXphdGlvbnNcbiAgICAgICAgcmV0dXJuIF9kZWxlZ2F0ZS5iaW5kKG51bGwsIGRvY3VtZW50KS5hcHBseShudWxsLCBhcmd1bWVudHMpO1xuICAgIH1cblxuICAgIC8vIEhhbmRsZSBTZWxlY3Rvci1iYXNlZCB1c2FnZVxuICAgIGlmICh0eXBlb2YgZWxlbWVudHMgPT09ICdzdHJpbmcnKSB7XG4gICAgICAgIGVsZW1lbnRzID0gZG9jdW1lbnQucXVlcnlTZWxlY3RvckFsbChlbGVtZW50cyk7XG4gICAgfVxuXG4gICAgLy8gSGFuZGxlIEFycmF5LWxpa2UgYmFzZWQgdXNhZ2VcbiAgICByZXR1cm4gQXJyYXkucHJvdG90eXBlLm1hcC5jYWxsKGVsZW1lbnRzLCBmdW5jdGlvbiAoZWxlbWVudCkge1xuICAgICAgICByZXR1cm4gX2RlbGVnYXRlKGVsZW1lbnQsIHNlbGVjdG9yLCB0eXBlLCBjYWxsYmFjaywgdXNlQ2FwdHVyZSk7XG4gICAgfSk7XG59XG5cbi8qKlxuICogRmluZHMgY2xvc2VzdCBtYXRjaCBhbmQgaW52b2tlcyBjYWxsYmFjay5cbiAqXG4gKiBAcGFyYW0ge0VsZW1lbnR9IGVsZW1lbnRcbiAqIEBwYXJhbSB7U3RyaW5nfSBzZWxlY3RvclxuICogQHBhcmFtIHtTdHJpbmd9IHR5cGVcbiAqIEBwYXJhbSB7RnVuY3Rpb259IGNhbGxiYWNrXG4gKiBAcmV0dXJuIHtGdW5jdGlvbn1cbiAqL1xuZnVuY3Rpb24gbGlzdGVuZXIoZWxlbWVudCwgc2VsZWN0b3IsIHR5cGUsIGNhbGxiYWNrKSB7XG4gICAgcmV0dXJuIGZ1bmN0aW9uKGUpIHtcbiAgICAgICAgZS5kZWxlZ2F0ZVRhcmdldCA9IGNsb3Nlc3QoZS50YXJnZXQsIHNlbGVjdG9yKTtcblxuICAgICAgICBpZiAoZS5kZWxlZ2F0ZVRhcmdldCkge1xuICAgICAgICAgICAgY2FsbGJhY2suY2FsbChlbGVtZW50LCBlKTtcbiAgICAgICAgfVxuICAgIH1cbn1cblxubW9kdWxlLmV4cG9ydHMgPSBkZWxlZ2F0ZTtcblxuXG4vKioqLyB9KSxcblxuLyoqKi8gODc5OlxuLyoqKi8gKGZ1bmN0aW9uKF9fdW51c2VkX3dlYnBhY2tfbW9kdWxlLCBleHBvcnRzKSB7XG5cbi8qKlxuICogQ2hlY2sgaWYgYXJndW1lbnQgaXMgYSBIVE1MIGVsZW1lbnQuXG4gKlxuICogQHBhcmFtIHtPYmplY3R9IHZhbHVlXG4gKiBAcmV0dXJuIHtCb29sZWFufVxuICovXG5leHBvcnRzLm5vZGUgPSBmdW5jdGlvbih2YWx1ZSkge1xuICAgIHJldHVybiB2YWx1ZSAhPT0gdW5kZWZpbmVkXG4gICAgICAgICYmIHZhbHVlIGluc3RhbmNlb2YgSFRNTEVsZW1lbnRcbiAgICAgICAgJiYgdmFsdWUubm9kZVR5cGUgPT09IDE7XG59O1xuXG4vKipcbiAqIENoZWNrIGlmIGFyZ3VtZW50IGlzIGEgbGlzdCBvZiBIVE1MIGVsZW1lbnRzLlxuICpcbiAqIEBwYXJhbSB7T2JqZWN0fSB2YWx1ZVxuICogQHJldHVybiB7Qm9vbGVhbn1cbiAqL1xuZXhwb3J0cy5ub2RlTGlzdCA9IGZ1bmN0aW9uKHZhbHVlKSB7XG4gICAgdmFyIHR5cGUgPSBPYmplY3QucHJvdG90eXBlLnRvU3RyaW5nLmNhbGwodmFsdWUpO1xuXG4gICAgcmV0dXJuIHZhbHVlICE9PSB1bmRlZmluZWRcbiAgICAgICAgJiYgKHR5cGUgPT09ICdbb2JqZWN0IE5vZGVMaXN0XScgfHwgdHlwZSA9PT0gJ1tvYmplY3QgSFRNTENvbGxlY3Rpb25dJylcbiAgICAgICAgJiYgKCdsZW5ndGgnIGluIHZhbHVlKVxuICAgICAgICAmJiAodmFsdWUubGVuZ3RoID09PSAwIHx8IGV4cG9ydHMubm9kZSh2YWx1ZVswXSkpO1xufTtcblxuLyoqXG4gKiBDaGVjayBpZiBhcmd1bWVudCBpcyBhIHN0cmluZy5cbiAqXG4gKiBAcGFyYW0ge09iamVjdH0gdmFsdWVcbiAqIEByZXR1cm4ge0Jvb2xlYW59XG4gKi9cbmV4cG9ydHMuc3RyaW5nID0gZnVuY3Rpb24odmFsdWUpIHtcbiAgICByZXR1cm4gdHlwZW9mIHZhbHVlID09PSAnc3RyaW5nJ1xuICAgICAgICB8fCB2YWx1ZSBpbnN0YW5jZW9mIFN0cmluZztcbn07XG5cbi8qKlxuICogQ2hlY2sgaWYgYXJndW1lbnQgaXMgYSBmdW5jdGlvbi5cbiAqXG4gKiBAcGFyYW0ge09iamVjdH0gdmFsdWVcbiAqIEByZXR1cm4ge0Jvb2xlYW59XG4gKi9cbmV4cG9ydHMuZm4gPSBmdW5jdGlvbih2YWx1ZSkge1xuICAgIHZhciB0eXBlID0gT2JqZWN0LnByb3RvdHlwZS50b1N0cmluZy5jYWxsKHZhbHVlKTtcblxuICAgIHJldHVybiB0eXBlID09PSAnW29iamVjdCBGdW5jdGlvbl0nO1xufTtcblxuXG4vKioqLyB9KSxcblxuLyoqKi8gMzcwOlxuLyoqKi8gKGZ1bmN0aW9uKG1vZHVsZSwgX191bnVzZWRfd2VicGFja19leHBvcnRzLCBfX3dlYnBhY2tfcmVxdWlyZV9fKSB7XG5cbnZhciBpcyA9IF9fd2VicGFja19yZXF1aXJlX18oODc5KTtcbnZhciBkZWxlZ2F0ZSA9IF9fd2VicGFja19yZXF1aXJlX18oNDM4KTtcblxuLyoqXG4gKiBWYWxpZGF0ZXMgYWxsIHBhcmFtcyBhbmQgY2FsbHMgdGhlIHJpZ2h0XG4gKiBsaXN0ZW5lciBmdW5jdGlvbiBiYXNlZCBvbiBpdHMgdGFyZ2V0IHR5cGUuXG4gKlxuICogQHBhcmFtIHtTdHJpbmd8SFRNTEVsZW1lbnR8SFRNTENvbGxlY3Rpb258Tm9kZUxpc3R9IHRhcmdldFxuICogQHBhcmFtIHtTdHJpbmd9IHR5cGVcbiAqIEBwYXJhbSB7RnVuY3Rpb259IGNhbGxiYWNrXG4gKiBAcmV0dXJuIHtPYmplY3R9XG4gKi9cbmZ1bmN0aW9uIGxpc3Rlbih0YXJnZXQsIHR5cGUsIGNhbGxiYWNrKSB7XG4gICAgaWYgKCF0YXJnZXQgJiYgIXR5cGUgJiYgIWNhbGxiYWNrKSB7XG4gICAgICAgIHRocm93IG5ldyBFcnJvcignTWlzc2luZyByZXF1aXJlZCBhcmd1bWVudHMnKTtcbiAgICB9XG5cbiAgICBpZiAoIWlzLnN0cmluZyh0eXBlKSkge1xuICAgICAgICB0aHJvdyBuZXcgVHlwZUVycm9yKCdTZWNvbmQgYXJndW1lbnQgbXVzdCBiZSBhIFN0cmluZycpO1xuICAgIH1cblxuICAgIGlmICghaXMuZm4oY2FsbGJhY2spKSB7XG4gICAgICAgIHRocm93IG5ldyBUeXBlRXJyb3IoJ1RoaXJkIGFyZ3VtZW50IG11c3QgYmUgYSBGdW5jdGlvbicpO1xuICAgIH1cblxuICAgIGlmIChpcy5ub2RlKHRhcmdldCkpIHtcbiAgICAgICAgcmV0dXJuIGxpc3Rlbk5vZGUodGFyZ2V0LCB0eXBlLCBjYWxsYmFjayk7XG4gICAgfVxuICAgIGVsc2UgaWYgKGlzLm5vZGVMaXN0KHRhcmdldCkpIHtcbiAgICAgICAgcmV0dXJuIGxpc3Rlbk5vZGVMaXN0KHRhcmdldCwgdHlwZSwgY2FsbGJhY2spO1xuICAgIH1cbiAgICBlbHNlIGlmIChpcy5zdHJpbmcodGFyZ2V0KSkge1xuICAgICAgICByZXR1cm4gbGlzdGVuU2VsZWN0b3IodGFyZ2V0LCB0eXBlLCBjYWxsYmFjayk7XG4gICAgfVxuICAgIGVsc2Uge1xuICAgICAgICB0aHJvdyBuZXcgVHlwZUVycm9yKCdGaXJzdCBhcmd1bWVudCBtdXN0IGJlIGEgU3RyaW5nLCBIVE1MRWxlbWVudCwgSFRNTENvbGxlY3Rpb24sIG9yIE5vZGVMaXN0Jyk7XG4gICAgfVxufVxuXG4vKipcbiAqIEFkZHMgYW4gZXZlbnQgbGlzdGVuZXIgdG8gYSBIVE1MIGVsZW1lbnRcbiAqIGFuZCByZXR1cm5zIGEgcmVtb3ZlIGxpc3RlbmVyIGZ1bmN0aW9uLlxuICpcbiAqIEBwYXJhbSB7SFRNTEVsZW1lbnR9IG5vZGVcbiAqIEBwYXJhbSB7U3RyaW5nfSB0eXBlXG4gKiBAcGFyYW0ge0Z1bmN0aW9ufSBjYWxsYmFja1xuICogQHJldHVybiB7T2JqZWN0fVxuICovXG5mdW5jdGlvbiBsaXN0ZW5Ob2RlKG5vZGUsIHR5cGUsIGNhbGxiYWNrKSB7XG4gICAgbm9kZS5hZGRFdmVudExpc3RlbmVyKHR5cGUsIGNhbGxiYWNrKTtcblxuICAgIHJldHVybiB7XG4gICAgICAgIGRlc3Ryb3k6IGZ1bmN0aW9uKCkge1xuICAgICAgICAgICAgbm9kZS5yZW1vdmVFdmVudExpc3RlbmVyKHR5cGUsIGNhbGxiYWNrKTtcbiAgICAgICAgfVxuICAgIH1cbn1cblxuLyoqXG4gKiBBZGQgYW4gZXZlbnQgbGlzdGVuZXIgdG8gYSBsaXN0IG9mIEhUTUwgZWxlbWVudHNcbiAqIGFuZCByZXR1cm5zIGEgcmVtb3ZlIGxpc3RlbmVyIGZ1bmN0aW9uLlxuICpcbiAqIEBwYXJhbSB7Tm9kZUxpc3R8SFRNTENvbGxlY3Rpb259IG5vZGVMaXN0XG4gKiBAcGFyYW0ge1N0cmluZ30gdHlwZVxuICogQHBhcmFtIHtGdW5jdGlvbn0gY2FsbGJhY2tcbiAqIEByZXR1cm4ge09iamVjdH1cbiAqL1xuZnVuY3Rpb24gbGlzdGVuTm9kZUxpc3Qobm9kZUxpc3QsIHR5cGUsIGNhbGxiYWNrKSB7XG4gICAgQXJyYXkucHJvdG90eXBlLmZvckVhY2guY2FsbChub2RlTGlzdCwgZnVuY3Rpb24obm9kZSkge1xuICAgICAgICBub2RlLmFkZEV2ZW50TGlzdGVuZXIodHlwZSwgY2FsbGJhY2spO1xuICAgIH0pO1xuXG4gICAgcmV0dXJuIHtcbiAgICAgICAgZGVzdHJveTogZnVuY3Rpb24oKSB7XG4gICAgICAgICAgICBBcnJheS5wcm90b3R5cGUuZm9yRWFjaC5jYWxsKG5vZGVMaXN0LCBmdW5jdGlvbihub2RlKSB7XG4gICAgICAgICAgICAgICAgbm9kZS5yZW1vdmVFdmVudExpc3RlbmVyKHR5cGUsIGNhbGxiYWNrKTtcbiAgICAgICAgICAgIH0pO1xuICAgICAgICB9XG4gICAgfVxufVxuXG4vKipcbiAqIEFkZCBhbiBldmVudCBsaXN0ZW5lciB0byBhIHNlbGVjdG9yXG4gKiBhbmQgcmV0dXJucyBhIHJlbW92ZSBsaXN0ZW5lciBmdW5jdGlvbi5cbiAqXG4gKiBAcGFyYW0ge1N0cmluZ30gc2VsZWN0b3JcbiAqIEBwYXJhbSB7U3RyaW5nfSB0eXBlXG4gKiBAcGFyYW0ge0Z1bmN0aW9ufSBjYWxsYmFja1xuICogQHJldHVybiB7T2JqZWN0fVxuICovXG5mdW5jdGlvbiBsaXN0ZW5TZWxlY3RvcihzZWxlY3RvciwgdHlwZSwgY2FsbGJhY2spIHtcbiAgICByZXR1cm4gZGVsZWdhdGUoZG9jdW1lbnQuYm9keSwgc2VsZWN0b3IsIHR5cGUsIGNhbGxiYWNrKTtcbn1cblxubW9kdWxlLmV4cG9ydHMgPSBsaXN0ZW47XG5cblxuLyoqKi8gfSksXG5cbi8qKiovIDgxNzpcbi8qKiovIChmdW5jdGlvbihtb2R1bGUpIHtcblxuZnVuY3Rpb24gc2VsZWN0KGVsZW1lbnQpIHtcbiAgICB2YXIgc2VsZWN0ZWRUZXh0O1xuXG4gICAgaWYgKGVsZW1lbnQubm9kZU5hbWUgPT09ICdTRUxFQ1QnKSB7XG4gICAgICAgIGVsZW1lbnQuZm9jdXMoKTtcblxuICAgICAgICBzZWxlY3RlZFRleHQgPSBlbGVtZW50LnZhbHVlO1xuICAgIH1cbiAgICBlbHNlIGlmIChlbGVtZW50Lm5vZGVOYW1lID09PSAnSU5QVVQnIHx8IGVsZW1lbnQubm9kZU5hbWUgPT09ICdURVhUQVJFQScpIHtcbiAgICAgICAgdmFyIGlzUmVhZE9ubHkgPSBlbGVtZW50Lmhhc0F0dHJpYnV0ZSgncmVhZG9ubHknKTtcblxuICAgICAgICBpZiAoIWlzUmVhZE9ubHkpIHtcbiAgICAgICAgICAgIGVsZW1lbnQuc2V0QXR0cmlidXRlKCdyZWFkb25seScsICcnKTtcbiAgICAgICAgfVxuXG4gICAgICAgIGVsZW1lbnQuc2VsZWN0KCk7XG4gICAgICAgIGVsZW1lbnQuc2V0U2VsZWN0aW9uUmFuZ2UoMCwgZWxlbWVudC52YWx1ZS5sZW5ndGgpO1xuXG4gICAgICAgIGlmICghaXNSZWFkT25seSkge1xuICAgICAgICAgICAgZWxlbWVudC5yZW1vdmVBdHRyaWJ1dGUoJ3JlYWRvbmx5Jyk7XG4gICAgICAgIH1cblxuICAgICAgICBzZWxlY3RlZFRleHQgPSBlbGVtZW50LnZhbHVlO1xuICAgIH1cbiAgICBlbHNlIHtcbiAgICAgICAgaWYgKGVsZW1lbnQuaGFzQXR0cmlidXRlKCdjb250ZW50ZWRpdGFibGUnKSkge1xuICAgICAgICAgICAgZWxlbWVudC5mb2N1cygpO1xuICAgICAgICB9XG5cbiAgICAgICAgdmFyIHNlbGVjdGlvbiA9IHdpbmRvdy5nZXRTZWxlY3Rpb24oKTtcbiAgICAgICAgdmFyIHJhbmdlID0gZG9jdW1lbnQuY3JlYXRlUmFuZ2UoKTtcblxuICAgICAgICByYW5nZS5zZWxlY3ROb2RlQ29udGVudHMoZWxlbWVudCk7XG4gICAgICAgIHNlbGVjdGlvbi5yZW1vdmVBbGxSYW5nZXMoKTtcbiAgICAgICAgc2VsZWN0aW9uLmFkZFJhbmdlKHJhbmdlKTtcblxuICAgICAgICBzZWxlY3RlZFRleHQgPSBzZWxlY3Rpb24udG9TdHJpbmcoKTtcbiAgICB9XG5cbiAgICByZXR1cm4gc2VsZWN0ZWRUZXh0O1xufVxuXG5tb2R1bGUuZXhwb3J0cyA9IHNlbGVjdDtcblxuXG4vKioqLyB9KSxcblxuLyoqKi8gMjc5OlxuLyoqKi8gKGZ1bmN0aW9uKG1vZHVsZSkge1xuXG5mdW5jdGlvbiBFICgpIHtcbiAgLy8gS2VlcCB0aGlzIGVtcHR5IHNvIGl0J3MgZWFzaWVyIHRvIGluaGVyaXQgZnJvbVxuICAvLyAodmlhIGh0dHBzOi8vZ2l0aHViLmNvbS9saXBzbWFjayBmcm9tIGh0dHBzOi8vZ2l0aHViLmNvbS9zY290dGNvcmdhbi90aW55LWVtaXR0ZXIvaXNzdWVzLzMpXG59XG5cbkUucHJvdG90eXBlID0ge1xuICBvbjogZnVuY3Rpb24gKG5hbWUsIGNhbGxiYWNrLCBjdHgpIHtcbiAgICB2YXIgZSA9IHRoaXMuZSB8fCAodGhpcy5lID0ge30pO1xuXG4gICAgKGVbbmFtZV0gfHwgKGVbbmFtZV0gPSBbXSkpLnB1c2goe1xuICAgICAgZm46IGNhbGxiYWNrLFxuICAgICAgY3R4OiBjdHhcbiAgICB9KTtcblxuICAgIHJldHVybiB0aGlzO1xuICB9LFxuXG4gIG9uY2U6IGZ1bmN0aW9uIChuYW1lLCBjYWxsYmFjaywgY3R4KSB7XG4gICAgdmFyIHNlbGYgPSB0aGlzO1xuICAgIGZ1bmN0aW9uIGxpc3RlbmVyICgpIHtcbiAgICAgIHNlbGYub2ZmKG5hbWUsIGxpc3RlbmVyKTtcbiAgICAgIGNhbGxiYWNrLmFwcGx5KGN0eCwgYXJndW1lbnRzKTtcbiAgICB9O1xuXG4gICAgbGlzdGVuZXIuXyA9IGNhbGxiYWNrXG4gICAgcmV0dXJuIHRoaXMub24obmFtZSwgbGlzdGVuZXIsIGN0eCk7XG4gIH0sXG5cbiAgZW1pdDogZnVuY3Rpb24gKG5hbWUpIHtcbiAgICB2YXIgZGF0YSA9IFtdLnNsaWNlLmNhbGwoYXJndW1lbnRzLCAxKTtcbiAgICB2YXIgZXZ0QXJyID0gKCh0aGlzLmUgfHwgKHRoaXMuZSA9IHt9KSlbbmFtZV0gfHwgW10pLnNsaWNlKCk7XG4gICAgdmFyIGkgPSAwO1xuICAgIHZhciBsZW4gPSBldnRBcnIubGVuZ3RoO1xuXG4gICAgZm9yIChpOyBpIDwgbGVuOyBpKyspIHtcbiAgICAgIGV2dEFycltpXS5mbi5hcHBseShldnRBcnJbaV0uY3R4LCBkYXRhKTtcbiAgICB9XG5cbiAgICByZXR1cm4gdGhpcztcbiAgfSxcblxuICBvZmY6IGZ1bmN0aW9uIChuYW1lLCBjYWxsYmFjaykge1xuICAgIHZhciBlID0gdGhpcy5lIHx8ICh0aGlzLmUgPSB7fSk7XG4gICAgdmFyIGV2dHMgPSBlW25hbWVdO1xuICAgIHZhciBsaXZlRXZlbnRzID0gW107XG5cbiAgICBpZiAoZXZ0cyAmJiBjYWxsYmFjaykge1xuICAgICAgZm9yICh2YXIgaSA9IDAsIGxlbiA9IGV2dHMubGVuZ3RoOyBpIDwgbGVuOyBpKyspIHtcbiAgICAgICAgaWYgKGV2dHNbaV0uZm4gIT09IGNhbGxiYWNrICYmIGV2dHNbaV0uZm4uXyAhPT0gY2FsbGJhY2spXG4gICAgICAgICAgbGl2ZUV2ZW50cy5wdXNoKGV2dHNbaV0pO1xuICAgICAgfVxuICAgIH1cblxuICAgIC8vIFJlbW92ZSBldmVudCBmcm9tIHF1ZXVlIHRvIHByZXZlbnQgbWVtb3J5IGxlYWtcbiAgICAvLyBTdWdnZXN0ZWQgYnkgaHR0cHM6Ly9naXRodWIuY29tL2xhemRcbiAgICAvLyBSZWY6IGh0dHBzOi8vZ2l0aHViLmNvbS9zY290dGNvcmdhbi90aW55LWVtaXR0ZXIvY29tbWl0L2M2ZWJmYWE5YmM5NzNiMzNkMTEwYTg0YTMwNzc0MmI3Y2Y5NGM5NTMjY29tbWl0Y29tbWVudC01MDI0OTEwXG5cbiAgICAobGl2ZUV2ZW50cy5sZW5ndGgpXG4gICAgICA/IGVbbmFtZV0gPSBsaXZlRXZlbnRzXG4gICAgICA6IGRlbGV0ZSBlW25hbWVdO1xuXG4gICAgcmV0dXJuIHRoaXM7XG4gIH1cbn07XG5cbm1vZHVsZS5leHBvcnRzID0gRTtcbm1vZHVsZS5leHBvcnRzLlRpbnlFbWl0dGVyID0gRTtcblxuXG4vKioqLyB9KVxuXG4vKioqKioqLyBcdH0pO1xuLyoqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKi9cbi8qKioqKiovIFx0Ly8gVGhlIG1vZHVsZSBjYWNoZVxuLyoqKioqKi8gXHR2YXIgX193ZWJwYWNrX21vZHVsZV9jYWNoZV9fID0ge307XG4vKioqKioqLyBcdFxuLyoqKioqKi8gXHQvLyBUaGUgcmVxdWlyZSBmdW5jdGlvblxuLyoqKioqKi8gXHRmdW5jdGlvbiBfX3dlYnBhY2tfcmVxdWlyZV9fKG1vZHVsZUlkKSB7XG4vKioqKioqLyBcdFx0Ly8gQ2hlY2sgaWYgbW9kdWxlIGlzIGluIGNhY2hlXG4vKioqKioqLyBcdFx0aWYoX193ZWJwYWNrX21vZHVsZV9jYWNoZV9fW21vZHVsZUlkXSkge1xuLyoqKioqKi8gXHRcdFx0cmV0dXJuIF9fd2VicGFja19tb2R1bGVfY2FjaGVfX1ttb2R1bGVJZF0uZXhwb3J0cztcbi8qKioqKiovIFx0XHR9XG4vKioqKioqLyBcdFx0Ly8gQ3JlYXRlIGEgbmV3IG1vZHVsZSAoYW5kIHB1dCBpdCBpbnRvIHRoZSBjYWNoZSlcbi8qKioqKiovIFx0XHR2YXIgbW9kdWxlID0gX193ZWJwYWNrX21vZHVsZV9jYWNoZV9fW21vZHVsZUlkXSA9IHtcbi8qKioqKiovIFx0XHRcdC8vIG5vIG1vZHVsZS5pZCBuZWVkZWRcbi8qKioqKiovIFx0XHRcdC8vIG5vIG1vZHVsZS5sb2FkZWQgbmVlZGVkXG4vKioqKioqLyBcdFx0XHRleHBvcnRzOiB7fVxuLyoqKioqKi8gXHRcdH07XG4vKioqKioqLyBcdFxuLyoqKioqKi8gXHRcdC8vIEV4ZWN1dGUgdGhlIG1vZHVsZSBmdW5jdGlvblxuLyoqKioqKi8gXHRcdF9fd2VicGFja19tb2R1bGVzX19bbW9kdWxlSWRdKG1vZHVsZSwgbW9kdWxlLmV4cG9ydHMsIF9fd2VicGFja19yZXF1aXJlX18pO1xuLyoqKioqKi8gXHRcbi8qKioqKiovIFx0XHQvLyBSZXR1cm4gdGhlIGV4cG9ydHMgb2YgdGhlIG1vZHVsZVxuLyoqKioqKi8gXHRcdHJldHVybiBtb2R1bGUuZXhwb3J0cztcbi8qKioqKiovIFx0fVxuLyoqKioqKi8gXHRcbi8qKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKiovXG4vKioqKioqLyBcdC8qIHdlYnBhY2svcnVudGltZS9jb21wYXQgZ2V0IGRlZmF1bHQgZXhwb3J0ICovXG4vKioqKioqLyBcdCFmdW5jdGlvbigpIHtcbi8qKioqKiovIFx0XHQvLyBnZXREZWZhdWx0RXhwb3J0IGZ1bmN0aW9uIGZvciBjb21wYXRpYmlsaXR5IHdpdGggbm9uLWhhcm1vbnkgbW9kdWxlc1xuLyoqKioqKi8gXHRcdF9fd2VicGFja19yZXF1aXJlX18ubiA9IGZ1bmN0aW9uKG1vZHVsZSkge1xuLyoqKioqKi8gXHRcdFx0dmFyIGdldHRlciA9IG1vZHVsZSAmJiBtb2R1bGUuX19lc01vZHVsZSA/XG4vKioqKioqLyBcdFx0XHRcdGZ1bmN0aW9uKCkgeyByZXR1cm4gbW9kdWxlWydkZWZhdWx0J107IH0gOlxuLyoqKioqKi8gXHRcdFx0XHRmdW5jdGlvbigpIHsgcmV0dXJuIG1vZHVsZTsgfTtcbi8qKioqKiovIFx0XHRcdF9fd2VicGFja19yZXF1aXJlX18uZChnZXR0ZXIsIHsgYTogZ2V0dGVyIH0pO1xuLyoqKioqKi8gXHRcdFx0cmV0dXJuIGdldHRlcjtcbi8qKioqKiovIFx0XHR9O1xuLyoqKioqKi8gXHR9KCk7XG4vKioqKioqLyBcdFxuLyoqKioqKi8gXHQvKiB3ZWJwYWNrL3J1bnRpbWUvZGVmaW5lIHByb3BlcnR5IGdldHRlcnMgKi9cbi8qKioqKiovIFx0IWZ1bmN0aW9uKCkge1xuLyoqKioqKi8gXHRcdC8vIGRlZmluZSBnZXR0ZXIgZnVuY3Rpb25zIGZvciBoYXJtb255IGV4cG9ydHNcbi8qKioqKiovIFx0XHRfX3dlYnBhY2tfcmVxdWlyZV9fLmQgPSBmdW5jdGlvbihleHBvcnRzLCBkZWZpbml0aW9uKSB7XG4vKioqKioqLyBcdFx0XHRmb3IodmFyIGtleSBpbiBkZWZpbml0aW9uKSB7XG4vKioqKioqLyBcdFx0XHRcdGlmKF9fd2VicGFja19yZXF1aXJlX18ubyhkZWZpbml0aW9uLCBrZXkpICYmICFfX3dlYnBhY2tfcmVxdWlyZV9fLm8oZXhwb3J0cywga2V5KSkge1xuLyoqKioqKi8gXHRcdFx0XHRcdE9iamVjdC5kZWZpbmVQcm9wZXJ0eShleHBvcnRzLCBrZXksIHsgZW51bWVyYWJsZTogdHJ1ZSwgZ2V0OiBkZWZpbml0aW9uW2tleV0gfSk7XG4vKioqKioqLyBcdFx0XHRcdH1cbi8qKioqKiovIFx0XHRcdH1cbi8qKioqKiovIFx0XHR9O1xuLyoqKioqKi8gXHR9KCk7XG4vKioqKioqLyBcdFxuLyoqKioqKi8gXHQvKiB3ZWJwYWNrL3J1bnRpbWUvaGFzT3duUHJvcGVydHkgc2hvcnRoYW5kICovXG4vKioqKioqLyBcdCFmdW5jdGlvbigpIHtcbi8qKioqKiovIFx0XHRfX3dlYnBhY2tfcmVxdWlyZV9fLm8gPSBmdW5jdGlvbihvYmosIHByb3ApIHsgcmV0dXJuIE9iamVjdC5wcm90b3R5cGUuaGFzT3duUHJvcGVydHkuY2FsbChvYmosIHByb3ApOyB9XG4vKioqKioqLyBcdH0oKTtcbi8qKioqKiovIFx0XG4vKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqL1xuLyoqKioqKi8gXHQvLyBtb2R1bGUgZXhwb3J0cyBtdXN0IGJlIHJldHVybmVkIGZyb20gcnVudGltZSBzbyBlbnRyeSBpbmxpbmluZyBpcyBkaXNhYmxlZFxuLyoqKioqKi8gXHQvLyBzdGFydHVwXG4vKioqKioqLyBcdC8vIExvYWQgZW50cnkgbW9kdWxlIGFuZCByZXR1cm4gZXhwb3J0c1xuLyoqKioqKi8gXHRyZXR1cm4gX193ZWJwYWNrX3JlcXVpcmVfXyg2ODYpO1xuLyoqKioqKi8gfSkoKVxuLmRlZmF1bHQ7XG59KTsiLCAiIWZ1bmN0aW9uKGUpe2lmKFwib2JqZWN0XCI9PXR5cGVvZiBleHBvcnRzJiZcInVuZGVmaW5lZFwiIT10eXBlb2YgbW9kdWxlKW1vZHVsZS5leHBvcnRzPWUoKTtlbHNlIGlmKFwiZnVuY3Rpb25cIj09dHlwZW9mIGRlZmluZSYmZGVmaW5lLmFtZClkZWZpbmUoW10sZSk7ZWxzZXsoXCJ1bmRlZmluZWRcIiE9dHlwZW9mIHdpbmRvdz93aW5kb3c6XCJ1bmRlZmluZWRcIiE9dHlwZW9mIGdsb2JhbD9nbG9iYWw6XCJ1bmRlZmluZWRcIiE9dHlwZW9mIHNlbGY/c2VsZjp0aGlzKS5iYXNpY0xpZ2h0Ym94PWUoKX19KChmdW5jdGlvbigpe3JldHVybiBmdW5jdGlvbiBlKG4sdCxvKXtmdW5jdGlvbiByKGMsdSl7aWYoIXRbY10pe2lmKCFuW2NdKXt2YXIgcz1cImZ1bmN0aW9uXCI9PXR5cGVvZiByZXF1aXJlJiZyZXF1aXJlO2lmKCF1JiZzKXJldHVybiBzKGMsITApO2lmKGkpcmV0dXJuIGkoYywhMCk7dmFyIGE9bmV3IEVycm9yKFwiQ2Fubm90IGZpbmQgbW9kdWxlICdcIitjK1wiJ1wiKTt0aHJvdyBhLmNvZGU9XCJNT0RVTEVfTk9UX0ZPVU5EXCIsYX12YXIgbD10W2NdPXtleHBvcnRzOnt9fTtuW2NdWzBdLmNhbGwobC5leHBvcnRzLChmdW5jdGlvbihlKXtyZXR1cm4gcihuW2NdWzFdW2VdfHxlKX0pLGwsbC5leHBvcnRzLGUsbix0LG8pfXJldHVybiB0W2NdLmV4cG9ydHN9Zm9yKHZhciBpPVwiZnVuY3Rpb25cIj09dHlwZW9mIHJlcXVpcmUmJnJlcXVpcmUsYz0wO2M8by5sZW5ndGg7YysrKXIob1tjXSk7cmV0dXJuIHJ9KHsxOltmdW5jdGlvbihlLG4sdCl7XCJ1c2Ugc3RyaWN0XCI7T2JqZWN0LmRlZmluZVByb3BlcnR5KHQsXCJfX2VzTW9kdWxlXCIse3ZhbHVlOiEwfSksdC5jcmVhdGU9dC52aXNpYmxlPXZvaWQgMDt2YXIgbz1mdW5jdGlvbihlKXt2YXIgbj1hcmd1bWVudHMubGVuZ3RoPjEmJnZvaWQgMCE9PWFyZ3VtZW50c1sxXSYmYXJndW1lbnRzWzFdLHQ9ZG9jdW1lbnQuY3JlYXRlRWxlbWVudChcImRpdlwiKTtyZXR1cm4gdC5pbm5lckhUTUw9ZS50cmltKCksITA9PT1uP3QuY2hpbGRyZW46dC5maXJzdENoaWxkfSxyPWZ1bmN0aW9uKGUsbil7dmFyIHQ9ZS5jaGlsZHJlbjtyZXR1cm4gMT09PXQubGVuZ3RoJiZ0WzBdLnRhZ05hbWU9PT1ufSxpPWZ1bmN0aW9uKGUpe3JldHVybiBudWxsIT0oZT1lfHxkb2N1bWVudC5xdWVyeVNlbGVjdG9yKFwiLmJhc2ljTGlnaHRib3hcIikpJiYhMD09PWUub3duZXJEb2N1bWVudC5ib2R5LmNvbnRhaW5zKGUpfTt0LnZpc2libGU9aTt0LmNyZWF0ZT1mdW5jdGlvbihlLG4pe3ZhciB0PWZ1bmN0aW9uKGUsbil7dmFyIHQ9bygnXFxuXFx0XFx0PGRpdiBjbGFzcz1cImJhc2ljTGlnaHRib3ggJy5jb25jYXQobi5jbGFzc05hbWUsJ1wiPlxcblxcdFxcdFxcdDxkaXYgY2xhc3M9XCJiYXNpY0xpZ2h0Ym94X19wbGFjZWhvbGRlclwiIHJvbGU9XCJkaWFsb2dcIj48L2Rpdj5cXG5cXHRcXHQ8L2Rpdj5cXG5cXHQnKSksaT10LnF1ZXJ5U2VsZWN0b3IoXCIuYmFzaWNMaWdodGJveF9fcGxhY2Vob2xkZXJcIik7ZS5mb3JFYWNoKChmdW5jdGlvbihlKXtyZXR1cm4gaS5hcHBlbmRDaGlsZChlKX0pKTt2YXIgYz1yKGksXCJJTUdcIiksdT1yKGksXCJWSURFT1wiKSxzPXIoaSxcIklGUkFNRVwiKTtyZXR1cm4hMD09PWMmJnQuY2xhc3NMaXN0LmFkZChcImJhc2ljTGlnaHRib3gtLWltZ1wiKSwhMD09PXUmJnQuY2xhc3NMaXN0LmFkZChcImJhc2ljTGlnaHRib3gtLXZpZGVvXCIpLCEwPT09cyYmdC5jbGFzc0xpc3QuYWRkKFwiYmFzaWNMaWdodGJveC0taWZyYW1lXCIpLHR9KGU9ZnVuY3Rpb24oZSl7dmFyIG49XCJzdHJpbmdcIj09dHlwZW9mIGUsdD1lIGluc3RhbmNlb2YgSFRNTEVsZW1lbnQ9PTE7aWYoITE9PT1uJiYhMT09PXQpdGhyb3cgbmV3IEVycm9yKFwiQ29udGVudCBtdXN0IGJlIGEgRE9NIGVsZW1lbnQvbm9kZSBvciBzdHJpbmdcIik7cmV0dXJuITA9PT1uP0FycmF5LmZyb20obyhlLCEwKSk6XCJURU1QTEFURVwiPT09ZS50YWdOYW1lP1tlLmNvbnRlbnQuY2xvbmVOb2RlKCEwKV06QXJyYXkuZnJvbShlLmNoaWxkcmVuKX0oZSksbj1mdW5jdGlvbigpe3ZhciBlPWFyZ3VtZW50cy5sZW5ndGg+MCYmdm9pZCAwIT09YXJndW1lbnRzWzBdP2FyZ3VtZW50c1swXTp7fTtpZihudWxsPT0oZT1PYmplY3QuYXNzaWduKHt9LGUpKS5jbG9zYWJsZSYmKGUuY2xvc2FibGU9ITApLG51bGw9PWUuY2xhc3NOYW1lJiYoZS5jbGFzc05hbWU9XCJcIiksbnVsbD09ZS5vblNob3cmJihlLm9uU2hvdz1mdW5jdGlvbigpe30pLG51bGw9PWUub25DbG9zZSYmKGUub25DbG9zZT1mdW5jdGlvbigpe30pLFwiYm9vbGVhblwiIT10eXBlb2YgZS5jbG9zYWJsZSl0aHJvdyBuZXcgRXJyb3IoXCJQcm9wZXJ0eSBgY2xvc2FibGVgIG11c3QgYmUgYSBib29sZWFuXCIpO2lmKFwic3RyaW5nXCIhPXR5cGVvZiBlLmNsYXNzTmFtZSl0aHJvdyBuZXcgRXJyb3IoXCJQcm9wZXJ0eSBgY2xhc3NOYW1lYCBtdXN0IGJlIGEgc3RyaW5nXCIpO2lmKFwiZnVuY3Rpb25cIiE9dHlwZW9mIGUub25TaG93KXRocm93IG5ldyBFcnJvcihcIlByb3BlcnR5IGBvblNob3dgIG11c3QgYmUgYSBmdW5jdGlvblwiKTtpZihcImZ1bmN0aW9uXCIhPXR5cGVvZiBlLm9uQ2xvc2UpdGhyb3cgbmV3IEVycm9yKFwiUHJvcGVydHkgYG9uQ2xvc2VgIG11c3QgYmUgYSBmdW5jdGlvblwiKTtyZXR1cm4gZX0obikpLGM9ZnVuY3Rpb24oZSl7cmV0dXJuITEhPT1uLm9uQ2xvc2UodSkmJmZ1bmN0aW9uKGUsbil7cmV0dXJuIGUuY2xhc3NMaXN0LnJlbW92ZShcImJhc2ljTGlnaHRib3gtLXZpc2libGVcIiksc2V0VGltZW91dCgoZnVuY3Rpb24oKXtyZXR1cm4hMT09PWkoZSl8fGUucGFyZW50RWxlbWVudC5yZW1vdmVDaGlsZChlKSxuKCl9KSw0MTApLCEwfSh0LChmdW5jdGlvbigpe2lmKFwiZnVuY3Rpb25cIj09dHlwZW9mIGUpcmV0dXJuIGUodSl9KSl9OyEwPT09bi5jbG9zYWJsZSYmdC5hZGRFdmVudExpc3RlbmVyKFwiY2xpY2tcIiwoZnVuY3Rpb24oZSl7ZS50YXJnZXQ9PT10JiZjKCl9KSk7dmFyIHU9e2VsZW1lbnQ6ZnVuY3Rpb24oKXtyZXR1cm4gdH0sdmlzaWJsZTpmdW5jdGlvbigpe3JldHVybiBpKHQpfSxzaG93OmZ1bmN0aW9uKGUpe3JldHVybiExIT09bi5vblNob3codSkmJmZ1bmN0aW9uKGUsbil7cmV0dXJuIGRvY3VtZW50LmJvZHkuYXBwZW5kQ2hpbGQoZSksc2V0VGltZW91dCgoZnVuY3Rpb24oKXtyZXF1ZXN0QW5pbWF0aW9uRnJhbWUoKGZ1bmN0aW9uKCl7cmV0dXJuIGUuY2xhc3NMaXN0LmFkZChcImJhc2ljTGlnaHRib3gtLXZpc2libGVcIiksbigpfSkpfSksMTApLCEwfSh0LChmdW5jdGlvbigpe2lmKFwiZnVuY3Rpb25cIj09dHlwZW9mIGUpcmV0dXJuIGUodSl9KSl9LGNsb3NlOmN9O3JldHVybiB1fX0se31dfSx7fSxbMV0pKDEpfSkpOyIsICJmdW5jdGlvbiBlKGUscil7KG51bGw9PXJ8fHI+ZS5sZW5ndGgpJiYocj1lLmxlbmd0aCk7Zm9yKHZhciBuPTAsdD1BcnJheShyKTtuPHI7bisrKXRbbl09ZVtuXTtyZXR1cm4gdH1mdW5jdGlvbiByKHIsbil7dmFyIHQ9XCJ1bmRlZmluZWRcIiE9dHlwZW9mIFN5bWJvbCYmcltTeW1ib2wuaXRlcmF0b3JdfHxyW1wiQEBpdGVyYXRvclwiXTtpZih0KXJldHVybih0PXQuY2FsbChyKSkubmV4dC5iaW5kKHQpO2lmKEFycmF5LmlzQXJyYXkocil8fCh0PWZ1bmN0aW9uKHIsbil7aWYocil7aWYoXCJzdHJpbmdcIj09dHlwZW9mIHIpcmV0dXJuIGUocixuKTt2YXIgdD17fS50b1N0cmluZy5jYWxsKHIpLnNsaWNlKDgsLTEpO3JldHVyblwiT2JqZWN0XCI9PT10JiZyLmNvbnN0cnVjdG9yJiYodD1yLmNvbnN0cnVjdG9yLm5hbWUpLFwiTWFwXCI9PT10fHxcIlNldFwiPT09dD9BcnJheS5mcm9tKHIpOlwiQXJndW1lbnRzXCI9PT10fHwvXig/OlVpfEkpbnQoPzo4fDE2fDMyKSg/OkNsYW1wZWQpP0FycmF5JC8udGVzdCh0KT9lKHIsbik6dm9pZCAwfX0ocikpfHxuKXt0JiYocj10KTt2YXIgbz0wO3JldHVybiBmdW5jdGlvbigpe3JldHVybiBvPj1yLmxlbmd0aD97ZG9uZTohMH06e2RvbmU6ITEsdmFsdWU6cltvKytdfX19dGhyb3cgbmV3IFR5cGVFcnJvcihcIkludmFsaWQgYXR0ZW1wdCB0byBpdGVyYXRlIG5vbi1pdGVyYWJsZSBpbnN0YW5jZS5cXG5JbiBvcmRlciB0byBiZSBpdGVyYWJsZSwgbm9uLWFycmF5IG9iamVjdHMgbXVzdCBoYXZlIGEgW1N5bWJvbC5pdGVyYXRvcl0oKSBtZXRob2QuXCIpfWZ1bmN0aW9uIG4oZSxyLG4sdCl7dmFyIG89e2hlYWRlcnM6e2FjY2VwdDpcIiovKlwifX07cmV0dXJuIHJ8fChvLm1vZGU9XCJuby1jb3JzXCIpLG4mJihvLmNyZWRlbnRpYWxzPVwiaW5jbHVkZVwiKSxvLnByaW9yaXR5PXQ/XCJoaWdoXCI6XCJsb3dcIix3aW5kb3cuZmV0Y2g/ZmV0Y2goZSxvKTpmdW5jdGlvbihlLHIpe3JldHVybiBuZXcgUHJvbWlzZShmdW5jdGlvbihuLHQsbyl7KG89bmV3IFhNTEh0dHBSZXF1ZXN0KS5vcGVuKFwiR0VUXCIsZSxvLndpdGhDcmVkZW50aWFscz1yKSxvLnNldFJlcXVlc3RIZWFkZXIoXCJBY2NlcHRcIixcIiovKlwiKSxvLm9ubG9hZD1mdW5jdGlvbigpezIwMD09PW8uc3RhdHVzP24oKTp0KCl9LG8uc2VuZCgpfSl9KGUsbil9dmFyIHQsbz0odD1kb2N1bWVudC5jcmVhdGVFbGVtZW50KFwibGlua1wiKSkucmVsTGlzdCYmdC5yZWxMaXN0LnN1cHBvcnRzJiZ0LnJlbExpc3Quc3VwcG9ydHMoXCJwcmVmZXRjaFwiKT9mdW5jdGlvbihlLHIpe3JldHVybiBuZXcgUHJvbWlzZShmdW5jdGlvbihuLHQsbyl7KG89ZG9jdW1lbnQuY3JlYXRlRWxlbWVudChcImxpbmtcIikpLnJlbD1cInByZWZldGNoXCIsby5ocmVmPWUsciYmby5zZXRBdHRyaWJ1dGUoXCJjcm9zc29yaWdpblwiLFwiYW5vbnltb3VzXCIpLG8ub25sb2FkPW4sby5vbmVycm9yPXQsZG9jdW1lbnQuaGVhZC5hcHBlbmQobyl9KX06bixpPXdpbmRvdy5yZXF1ZXN0SWRsZUNhbGxiYWNrfHxmdW5jdGlvbihlKXt2YXIgcj1EYXRlLm5vdygpO3JldHVybiBzZXRUaW1lb3V0KGZ1bmN0aW9uKCl7ZSh7ZGlkVGltZW91dDohMSx0aW1lUmVtYWluaW5nOmZ1bmN0aW9uKCl7cmV0dXJuIE1hdGgubWF4KDAsNTAtKERhdGUubm93KCktcikpfX0pfSwxKX0sYT1uZXcgU2V0LGM9bmV3IFNldCxzPSExO2Z1bmN0aW9uIHUoZSxyKXtyZXR1cm4gQXJyYXkuaXNBcnJheShyKT9yLnNvbWUoZnVuY3Rpb24ocil7cmV0dXJuIHUoZSxyKX0pOihyLnRlc3R8fHIpLmNhbGwocixlLmhyZWYsZSl9ZnVuY3Rpb24gbChlKXtyZXR1cm4hZXx8KGUuc2F2ZURhdGE/bmV3IEVycm9yKFwiU2F2ZS1EYXRhIGlzIGVuYWJsZWRcIik6IS8yZy8udGVzdChlLmVmZmVjdGl2ZVR5cGUpfHxuZXcgRXJyb3IoXCJuZXR3b3JrIGNvbmRpdGlvbnMgYXJlIHBvb3JcIikpfWZ1bmN0aW9uIGYoZSl7aWYodm9pZCAwPT09ZSYmKGU9e30pLCF3aW5kb3cuSW50ZXJzZWN0aW9uT2JzZXJ2ZXJ8fCEoXCJpc0ludGVyc2VjdGluZ1wiaW4gSW50ZXJzZWN0aW9uT2JzZXJ2ZXJFbnRyeS5wcm90b3R5cGUpKXJldHVybiBmdW5jdGlvbigpe307dmFyIG49ZnVuY3Rpb24oZSl7ZT1lfHwxO3ZhciByPVtdLG49MDtmdW5jdGlvbiB0KCl7bjxlJiZyLmxlbmd0aD4wJiYoci5zaGlmdCgpKCksbisrKX1yZXR1cm5bZnVuY3Rpb24oZSl7ci5wdXNoKGUpPjF8fHQoKX0sZnVuY3Rpb24oKXtuLS0sdCgpfV19KGUudGhyb3R0bGV8fDEvMCksdD1uWzBdLG89blsxXSxsPWUubGltaXR8fDEvMCxmPWUudGhyZXNob2xkfHwwLGg9ZS5vcmlnaW5zfHxbbG9jYXRpb24uaG9zdG5hbWVdLG09ZS5pZ25vcmVzfHxbXSxwPWUuZGVsYXl8fDAsZz1uZXcgTWFwLHc9bmV3IE1hcCx5PWUudGltZW91dEZufHxpLEU9XCJmdW5jdGlvblwiPT10eXBlb2YgZS5ocmVmRm4mJmUuaHJlZkZuLGI9ZS5wcmVyZW5kZXJ8fCExO3M9ZS5wcmVyZW5kZXJBbmRQcmVmZXRjaHx8ITE7dmFyIEE9bmV3IEludGVyc2VjdGlvbk9ic2VydmVyKGZ1bmN0aW9uKG4pe2Zvcih2YXIgaSx1PWZ1bmN0aW9uKCl7dmFyIG49aS52YWx1ZSx1PWcuZ2V0KG4udGFyZ2V0LmhyZWYpfHxuZXcgU2V0O2cuc2V0KG4udGFyZ2V0LmhyZWYsdSksbi5pc0ludGVyc2VjdGluZz8obj1uLnRhcmdldCx1LmFkZChuKSxmdW5jdGlvbihlLHIpe3I/c2V0VGltZW91dChlLHIpOmUoKX0oZnVuY3Rpb24oKXt1JiZ1LnNpemUmJihifHxzfHxBLnVub2JzZXJ2ZShuKSwoc3x8YikmJmMuc2l6ZTxsJiYhdy5oYXMobi5ocmVmKT92KEU/RShuKTpuLmhyZWYsZS5lYWdlcm5lc3MpLnRoZW4oZnVuY3Rpb24oZSl7Zm9yKHZhciBuLHQ9cihlKTshKG49dCgpKS5kb25lOyl7dmFyIG89bi52YWx1ZSxpPW9bMF0sYT1vWzFdO3cuc2V0KGksYSl9fSkuY2F0Y2goZnVuY3Rpb24ocil7aWYoIWUub25FcnJvcil0aHJvdyByO2Uub25FcnJvcihyKX0pOmEuc2l6ZTxsJiYhYiYmdChmdW5jdGlvbigpe2QoRT9FKG4pOm4uaHJlZixlLnByaW9yaXR5LGUuY2hlY2tBY2Nlc3NDb250cm9sQWxsb3dPcmlnaW4sZS5jaGVja0FjY2Vzc0NvbnRyb2xBbGxvd0NyZWRlbnRpYWxzLGUub25seU9uTW91c2VvdmVyKS50aGVuKG8pLmNhdGNoKGZ1bmN0aW9uKHIpe28oKSxlLm9uRXJyb3ImJmUub25FcnJvcihyKX0pfSkpfSxwKSk6KG49bi50YXJnZXQsdS5kZWxldGUobiksdy5oYXMobi5ocmVmKSYmdy5zZXQoZnVuY3Rpb24oZSxyKXt2YXIgbj1lLmdldChyKTt0cnl7bi5yZW1vdmUoKSxlLmRlbGV0ZShyKX1jYXRjaChlKXtyZXR1cm4gZX1yZXR1cm4gZX0odyxuLmhyZWYpKSl9LGY9cihuKTshKGk9ZigpKS5kb25lOyl1KCl9LHt0aHJlc2hvbGQ6Zn0pO3JldHVybiB5KGZ1bmN0aW9uKCl7Zm9yKHZhciBuLHQ9cihlLmVsJiZlLmVsLmxlbmd0aD4wJiZcIkFcIj09PWUuZWxbMF0ubm9kZU5hbWU/ZS5lbDooZS5lbHx8ZG9jdW1lbnQpLnF1ZXJ5U2VsZWN0b3JBbGwoXCJhXCIpKTshKG49dCgpKS5kb25lOyl7dmFyIG89bi52YWx1ZTtoLmxlbmd0aCYmIWguaW5jbHVkZXMoby5ob3N0bmFtZSl8fHUobyxtKXx8QS5vYnNlcnZlKG8pfX0se3RpbWVvdXQ6ZS50aW1lb3V0fHwyZTN9KSxmdW5jdGlvbigpe2EuY2xlYXIoKSxBLmRpc2Nvbm5lY3QoKX19ZnVuY3Rpb24gZChlLHQsaSx1LGYpe3ZhciBkPWwobmF2aWdhdG9yLmNvbm5lY3Rpb24pO3JldHVybiBkIGluc3RhbmNlb2YgRXJyb3I/UHJvbWlzZS5yZWplY3QobmV3IEVycm9yKFwiQ2Fubm90IHByZWZldGNoLCBcIitkLm1lc3NhZ2UpKTooYy5zaXplPjAmJiFzJiZjb25zb2xlLndhcm4oXCJbV2FybmluZ10gWW91IGFyZSB1c2luZyBib3RoIHByZWZldGNoaW5nIGFuZCBwcmVyZW5kZXJpbmcgb24gdGhlIHNhbWUgZG9jdW1lbnRcIiksUHJvbWlzZS5hbGwoW2VdLmZsYXQoKS5tYXAoZnVuY3Rpb24oZSl7cmV0dXJuIGEuaGFzKGUpP1tdOihhLmFkZChlKSxmdW5jdGlvbihlLG4sdCl7Zm9yKHZhciBvPWFyZ3VtZW50cy5sZW5ndGgsaT1uZXcgQXJyYXkobz4zP28tMzowKSxhPTM7YTxvO2ErKylpW2EtM109YXJndW1lbnRzW2FdO2lmKCF0KXJldHVybiBlLmFwcGx5KHZvaWQgMCxbbl0uY29uY2F0KGkpKTtmb3IodmFyIGMscz1kb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKCdhW2hyZWY9XCInK24rJ1wiXScpLHU9bmV3IE1hcCxsPWZ1bmN0aW9uKCl7dmFyIHI9Yy52YWx1ZSx0PWZ1bmN0aW9uKCl7dmFyIGE9c2V0VGltZW91dChmdW5jdGlvbigpe3JldHVybiByLnJlbW92ZUV2ZW50TGlzdGVuZXIoXCJtb3VzZWVudGVyXCIsdCksci5yZW1vdmVFdmVudExpc3RlbmVyKFwibW91c2VsZWF2ZVwiLG8pLGUuYXBwbHkodm9pZCAwLFtuXS5jb25jYXQoaSkpfSwyMDApO3Uuc2V0KHIsYSl9LG89ZnVuY3Rpb24oKXt2YXIgZT11LmdldChyKTtlJiYoY2xlYXJUaW1lb3V0KGUpLHUuZGVsZXRlKHIpKX07ci5hZGRFdmVudExpc3RlbmVyKFwibW91c2VlbnRlclwiLHQpLHIuYWRkRXZlbnRMaXN0ZW5lcihcIm1vdXNlbGVhdmVcIixvKX0sZj1yKHMpOyEoYz1mKCkpLmRvbmU7KWwoKX0odD9uOm8sbmV3IFVSTChlLGxvY2F0aW9uLmhyZWYpLnRvU3RyaW5nKCksZixpLHUsdCkpfSkpKX1mdW5jdGlvbiB2KGUsbil7dm9pZCAwPT09biYmKG49XCJpbW1lZGlhdGVcIiksZT1bZV0uZmxhdCgpO3ZhciB0PWwobmF2aWdhdG9yLmNvbm5lY3Rpb24pO2lmKHQgaW5zdGFuY2VvZiBFcnJvcilyZXR1cm4gUHJvbWlzZS5yZWplY3QobmV3IEVycm9yKFwiQ2Fubm90IHByZXJlbmRlciwgXCIrdC5tZXNzYWdlKSk7aWYoIUhUTUxTY3JpcHRFbGVtZW50LnN1cHBvcnRzKFwic3BlY3VsYXRpb25ydWxlc1wiKSlyZXR1cm4gZChlLCEwLCExLCExLFwibW9kZXJhdGVcIj09PW58fFwiY29uc2VydmF0aXZlXCI9PT1uKSxQcm9taXNlLnJlamVjdChuZXcgRXJyb3IoXCJUaGlzIGJyb3dzZXIgZG9lcyBub3Qgc3VwcG9ydCB0aGUgc3BlY3VsYXRpb24gcnVsZXMgQVBJLiBGYWxsaW5nIGJhY2sgdG8gcHJlZmV0Y2guXCIpKTtmb3IodmFyIG8saT1yKGUpOyEobz1pKCkpLmRvbmU7KXt2YXIgdT1vLnZhbHVlO2MuYWRkKHUpfWEuc2l6ZT4wJiYhcyYmY29uc29sZS53YXJuKFwiW1dhcm5pbmddIFlvdSBhcmUgdXNpbmcgYm90aCBwcmVmZXRjaGluZyBhbmQgcHJlcmVuZGVyaW5nIG9uIHRoZSBzYW1lIGRvY3VtZW50XCIpO3ZhciBmPWZ1bmN0aW9uKGUsbil7dmFyIHQ9bmV3IE1hcDt0cnl7Zm9yKHZhciBvLGk9cihlKTshKG89aSgpKS5kb25lOyl7dmFyIGE9by52YWx1ZSxjPWRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoXCJzY3JpcHRcIik7Yy50eXBlPVwic3BlY3VsYXRpb25ydWxlc1wiLGMudGV4dD1KU09OLnN0cmluZ2lmeSh7cHJlcmVuZGVyOlt7c291cmNlOlwibGlzdFwiLHVybHM6W2FdLGVhZ2VybmVzczpufV19KSxkb2N1bWVudC5oZWFkLmFwcGVuZChjKSx0LnNldChhLGMpfX1jYXRjaChlKXtyZXR1cm4gZX1yZXR1cm4gdH0oZSxuKTtyZXR1cm4gZi5zaXplPjA/UHJvbWlzZS5yZXNvbHZlKGYpOlByb21pc2UucmVqZWN0KGYpfWV4cG9ydHtmIGFzIGxpc3RlbixkIGFzIHByZWZldGNoLHYgYXMgcHJlcmVuZGVyfTtcbiIsICIvLyBodHRwczovL2dpdGh1Yi5jb20vR29vZ2xlQ2hyb21lTGFicy9xdWlja2xpbmtcbmltcG9ydCB7IGxpc3RlbiB9IGZyb20gJ3F1aWNrbGluay9kaXN0L3F1aWNrbGluay5tanMnO1xubGlzdGVuKHtcbiAgaWdub3JlczogW1xuICAgIC9cXC9hcGlcXC8/LyxcbiAgICAodXJpKSA9PiB1cmkuaW5jbHVkZXMoJy56aXAnKSxcbiAgICAodXJpLCBlbGVtKSA9PiBlbGVtLmhhc0F0dHJpYnV0ZSgnbm9wcmVmZXRjaCcpLFxuICAgICh1cmksIGVsZW0pID0+IGVsZW0uaGFzaCAmJiBlbGVtLnBhdGhuYW1lID09PSB3aW5kb3cubG9jYXRpb24ucGF0aG5hbWUsXG4gIF0sXG59KTtcblxuLy8gaHR0cHM6Ly9naXRodWIuY29tL2FGYXJrYXMvbGF6eXNpemVzL3RyZWUvZ2gtcGFnZXMvcGx1Z2lucy9uYXRpdmUtbG9hZGluZ1xuaW1wb3J0IGxhenlTaXplcyBmcm9tICdsYXp5c2l6ZXMnO1xuaW1wb3J0ICdsYXp5c2l6ZXMvcGx1Z2lucy9uYXRpdmUtbG9hZGluZy9scy5uYXRpdmUtbG9hZGluZyc7XG5cbnRyeSB7XG4gIGxhenlTaXplcy5jZmcubmF0aXZlTG9hZGluZyA9IHtcbiAgICBzZXRMb2FkaW5nQXR0cmlidXRlOiB0cnVlLFxuICAgIGRpc2FibGVMaXN0ZW5lcnM6IHtcbiAgICAgIHNjcm9sbDogdHJ1ZSxcbiAgICB9LFxuICB9O1xuXG4gIC8vIEFkZGl0aW9uYWwgcGVyZm9ybWFuY2Ugb3B0aW1pemF0aW9uc1xuICBsYXp5U2l6ZXMuY2ZnLmxhenlDbGFzcyA9ICdsYXp5bG9hZCc7XG4gIGxhenlTaXplcy5jZmcubG9hZGVkQ2xhc3MgPSAnbGF6eWxvYWRlZCc7XG4gIGxhenlTaXplcy5jZmcubG9hZGluZ0NsYXNzID0gJ2xhenlsb2FkaW5nJztcbiAgbGF6eVNpemVzLmNmZy5leHBGYWN0b3IgPSAxLjU7IC8vIFByZWxvYWQgaW1hZ2VzIDEuNXggdmlld3BvcnQgYmVmb3JlIHRoZXkgZW50ZXJcblxuICBpZiAodHlwZW9mIGRvY3VtZW50ICE9PSAndW5kZWZpbmVkJykge1xuICAgIGRvY3VtZW50LmRvY3VtZW50RWxlbWVudC5jbGFzc0xpc3QuYWRkKCdsYXp5c2l6ZXMtZW5hYmxlZCcpO1xuICB9XG59IGNhdGNoIChlcnJvcikge1xuICBpZiAodHlwZW9mIGNvbnNvbGUgIT09ICd1bmRlZmluZWQnICYmIGNvbnNvbGUud2Fybikge1xuICAgIGNvbnNvbGUud2FybignbGF6eVNpemVzIGluaXRpYWxpemF0aW9uIGZhaWxlZDonLCBlcnJvcik7XG4gIH1cbn1cbiIsICIvKiFcclxuICogY2xpcGJvYXJkLmpzIGZvciBCb290c3RyYXAgYmFzZWQgVGh1bGl0ZSBzaXRlc1xyXG4gKiBDb3B5cmlnaHQgMjAyMS0yMDI0IFRodWxpdGVcclxuICogTGljZW5zZWQgdW5kZXIgdGhlIE1JVCBMaWNlbnNlXHJcbiAqL1xyXG5cclxuaW1wb3J0IENsaXBib2FyZCBmcm9tICdjbGlwYm9hcmQnO1xyXG5cclxuKCgpID0+IHtcclxuICAndXNlIHN0cmljdCc7XHJcblxyXG4gIHZhciBjYiA9IGRvY3VtZW50LmdldEVsZW1lbnRzQnlDbGFzc05hbWUoJ2hpZ2hsaWdodCcpO1xyXG5cclxuICBmb3IgKHZhciBpID0gMDsgaSA8IGNiLmxlbmd0aDsgKytpKSB7XHJcbiAgICB2YXIgZWxlbWVudCA9IGNiW2ldO1xyXG4gICAgZWxlbWVudC5pbnNlcnRBZGphY2VudEhUTUwoXHJcbiAgICAgICdhZnRlcmJlZ2luJyxcclxuICAgICAgJzxkaXYgY2xhc3M9XCJjb3B5XCI+PGJ1dHRvbiB0aXRsZT1cIkNvcHkgdG8gY2xpcGJvYXJkXCIgY2xhc3M9XCJidG4tY29weVwiIGFyaWEtbGFiZWw9XCJDbGlwYm9hcmQgYnV0dG9uXCI+PGRpdj48L2Rpdj48L2J1dHRvbj48L2Rpdj4nXHJcbiAgICApO1xyXG4gIH1cclxuXHJcbiAgdmFyIGNsaXBib2FyZCA9IG5ldyBDbGlwYm9hcmQoJy5idG4tY29weScsIHtcclxuICAgIHRhcmdldDogZnVuY3Rpb24gKHRyaWdnZXIpIHtcclxuICAgICAgcmV0dXJuIHRyaWdnZXIucGFyZW50Tm9kZS5uZXh0RWxlbWVudFNpYmxpbmc7XHJcbiAgICB9LFxyXG4gIH0pO1xyXG5cclxuICBjbGlwYm9hcmQub24oJ3N1Y2Nlc3MnLCBmdW5jdGlvbiAoZSkge1xyXG4gICAgLypcclxuICAgICAgY29uc29sZS5pbmZvKCdBY3Rpb246JywgZS5hY3Rpb24pO1xyXG4gICAgICBjb25zb2xlLmluZm8oJ1RleHQ6JywgZS50ZXh0KTtcclxuICAgICAgY29uc29sZS5pbmZvKCdUcmlnZ2VyOicsIGUudHJpZ2dlcik7XHJcbiAgICAgICovXHJcblxyXG4gICAgZS5jbGVhclNlbGVjdGlvbigpO1xyXG4gIH0pO1xyXG5cclxuICBjbGlwYm9hcmQub24oJ2Vycm9yJywgZnVuY3Rpb24gKGUpIHtcclxuICAgIGNvbnNvbGUuZXJyb3IoJ0FjdGlvbjonLCBlLmFjdGlvbik7XHJcbiAgICBjb25zb2xlLmVycm9yKCdUcmlnZ2VyOicsIGUudHJpZ2dlcik7XHJcbiAgfSk7XHJcbn0pKCk7XHJcbiIsICJjb25zdCB0b3BCdXR0b24gPSBkb2N1bWVudC5nZXRFbGVtZW50QnlJZCgndG9Ub3AnKTtcclxuXHJcbmlmICh0b3BCdXR0b24gIT09IG51bGwpIHtcclxuICB0b3BCdXR0b24uY2xhc3NMaXN0LnJlbW92ZSgnZmFkZScpO1xyXG4gIHdpbmRvdy5vbnNjcm9sbCA9IGZ1bmN0aW9uICgpIHtcclxuICAgIHNjcm9sbEZ1bmN0aW9uKCk7XHJcbiAgfTtcclxuXHJcbiAgdG9wQnV0dG9uLmFkZEV2ZW50TGlzdGVuZXIoJ2NsaWNrJywgdG9wRnVuY3Rpb24pO1xyXG59XHJcblxyXG5mdW5jdGlvbiBzY3JvbGxGdW5jdGlvbigpIHtcclxuICBpZiAoZG9jdW1lbnQuYm9keS5zY3JvbGxUb3AgPiAyNzAgfHwgZG9jdW1lbnQuZG9jdW1lbnRFbGVtZW50LnNjcm9sbFRvcCA+IDI3MCkge1xyXG4gICAgdG9wQnV0dG9uLmNsYXNzTGlzdC5hZGQoJ2ZhZGUnKTtcclxuICB9IGVsc2Uge1xyXG4gICAgdG9wQnV0dG9uLmNsYXNzTGlzdC5yZW1vdmUoJ2ZhZGUnKTtcclxuICB9XHJcbn1cclxuXHJcbmZ1bmN0aW9uIHRvcEZ1bmN0aW9uKCkge1xyXG4gIGRvY3VtZW50LmJvZHkuc2Nyb2xsVG9wID0gMDtcclxuICBkb2N1bWVudC5kb2N1bWVudEVsZW1lbnQuc2Nyb2xsVG9wID0gMDtcclxufVxyXG4iLCAiLy8gQmFzZWQgb246IGh0dHBzOi8vZ2l0aHViLmNvbS9nb2h1Z29pby9odWdvRG9jcy9ibG9iL21hc3Rlci9fdmVuZG9yL2dpdGh1Yi5jb20vZ29odWdvaW8vZ29odWdvaW9UaGVtZS9hc3NldHMvanMvdGFicy5qc1xyXG5cclxuLyoqXHJcbiAqIFNjcmlwdHMgd2hpY2ggbWFuYWdlcyBDb2RlIFRvZ2dsZSB0YWJzLlxyXG4gKi9cclxudmFyIGk7XHJcbi8vIHN0b3JlIHRhYnMgdmFyaWFibGVcclxudmFyIGFsbFRhYnMgPSBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKCdbZGF0YS10b2dnbGUtdGFiXScpO1xyXG52YXIgYWxsUGFuZXMgPSBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKCdbZGF0YS1wYW5lXScpO1xyXG5cclxuZnVuY3Rpb24gdG9nZ2xlVGFicyhldmVudCkge1xyXG4gIGlmIChldmVudC50YXJnZXQpIHtcclxuICAgIGV2ZW50LnByZXZlbnREZWZhdWx0KCk7XHJcbiAgICB2YXIgY2xpY2tlZFRhYiA9IGV2ZW50LmN1cnJlbnRUYXJnZXQ7XHJcbiAgICB2YXIgdGFyZ2V0S2V5ID0gY2xpY2tlZFRhYi5nZXRBdHRyaWJ1dGUoJ2RhdGEtdG9nZ2xlLXRhYicpO1xyXG4gIH0gZWxzZSB7XHJcbiAgICB2YXIgdGFyZ2V0S2V5ID0gZXZlbnQ7XHJcbiAgfVxyXG4gIC8vIFdlIHN0b3JlIHRoZSBjb25maWcgbGFuZ3VhZ2Ugc2VsZWN0ZWQgaW4gdXNlcnMnIGxvY2FsU3RvcmFnZVxyXG4gIGlmICh3aW5kb3cubG9jYWxTdG9yYWdlKSB7XHJcbiAgICB3aW5kb3cubG9jYWxTdG9yYWdlLnNldEl0ZW0oJ2NvbmZpZ0xhbmdQcmVmJywgdGFyZ2V0S2V5KTtcclxuICB9XHJcbiAgdmFyIHNlbGVjdGVkVGFicyA9IGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoJ1tkYXRhLXRvZ2dsZS10YWI9JyArIHRhcmdldEtleSArICddJyk7XHJcbiAgdmFyIHNlbGVjdGVkUGFuZXMgPSBkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKCdbZGF0YS1wYW5lPScgKyB0YXJnZXRLZXkgKyAnXScpO1xyXG5cclxuICBmb3IgKHZhciBpID0gMDsgaSA8IGFsbFRhYnMubGVuZ3RoOyBpKyspIHtcclxuICAgIGFsbFRhYnNbaV0uY2xhc3NMaXN0LnJlbW92ZSgnYWN0aXZlJyk7XHJcbiAgICBhbGxQYW5lc1tpXS5jbGFzc0xpc3QucmVtb3ZlKCdhY3RpdmUnKTtcclxuICB9XHJcblxyXG4gIGZvciAodmFyIGkgPSAwOyBpIDwgc2VsZWN0ZWRUYWJzLmxlbmd0aDsgaSsrKSB7XHJcbiAgICBzZWxlY3RlZFRhYnNbaV0uY2xhc3NMaXN0LmFkZCgnYWN0aXZlJyk7XHJcbiAgICBzZWxlY3RlZFBhbmVzW2ldLmNsYXNzTGlzdC5hZGQoJ3Nob3cnLCAnYWN0aXZlJyk7XHJcbiAgfVxyXG59XHJcblxyXG5mb3IgKGkgPSAwOyBpIDwgYWxsVGFicy5sZW5ndGg7IGkrKykge1xyXG4gIGFsbFRhYnNbaV0uYWRkRXZlbnRMaXN0ZW5lcignY2xpY2snLCB0b2dnbGVUYWJzKTtcclxufVxyXG4vLyBVcG9uIHBhZ2UgbG9hZCwgaWYgdXNlciBoYXMgYSBwcmVmZXJyZWQgbGFuZ3VhZ2UgaW4gaXRzIGxvY2FsU3RvcmFnZSwgdGFicyBhcmUgc2V0IHRvIGl0LlxyXG5pZiAod2luZG93LmxvY2FsU3RvcmFnZS5nZXRJdGVtKCdjb25maWdMYW5nUHJlZicpKSB7XHJcbiAgdG9nZ2xlVGFicyh3aW5kb3cubG9jYWxTdG9yYWdlLmdldEl0ZW0oJ2NvbmZpZ0xhbmdQcmVmJykpO1xyXG59XHJcbiIsICIvLyBDbG9zZSBtb2JpbGUgVE9DIGRldGFpbHMgd2hlbiBjbGlja2luZyBvbiBhIGxpbmtcbmRvY3VtZW50LmFkZEV2ZW50TGlzdGVuZXIoJ2NsaWNrJywgZnVuY3Rpb24gKGUpIHtcbiAgLy8gQ2hlY2sgaWYgdGhlIGNsaWNrZWQgZWxlbWVudCBpcyBhIGxpbmsgd2l0aGluIHRoZSBtb2JpbGUgVE9DXG4gIGNvbnN0IHRvY01vYmlsZSA9IGUudGFyZ2V0LmNsb3Nlc3QoJy50b2MtbW9iaWxlJyk7XG4gIGlmICh0b2NNb2JpbGUgJiYgZS50YXJnZXQudGFnTmFtZSA9PT0gJ0EnKSB7XG4gICAgLy8gRmluZCB0aGUgZGV0YWlscyBlbGVtZW50IHdpdGhpbiB0aGUgbW9iaWxlIFRPQ1xuICAgIGNvbnN0IGRldGFpbHMgPSB0b2NNb2JpbGUucXVlcnlTZWxlY3RvcignZGV0YWlscycpO1xuICAgIGlmIChkZXRhaWxzICYmIGRldGFpbHMub3Blbikge1xuICAgICAgLy8gQ2xvc2UgdGhlIGRldGFpbHMgZWxlbWVudFxuICAgICAgZGV0YWlscy5vcGVuID0gZmFsc2U7XG4gICAgfVxuICB9XG59KTtcbiIsICJjb25zdCBjb3B5TWFya2Rvd25CdG4gPSBkb2N1bWVudC5nZXRFbGVtZW50QnlJZCgnY29weS1tYXJrZG93bicpO1xyXG5pZiAoY29weU1hcmtkb3duQnRuKSB7XHJcbiAgY29weU1hcmtkb3duQnRuLmFkZEV2ZW50TGlzdGVuZXIoJ2NsaWNrJywgZnVuY3Rpb24gKGUpIHtcclxuICAgIGUucHJldmVudERlZmF1bHQoKTsgLy8gUHJldmVudCBkZWZhdWx0IGJ1dHRvbiBiZWhhdmlvclxyXG4gICAgY29uc3QgYnV0dG9uID0gdGhpcztcclxuXHJcbiAgICAvLyBHZXQgdGhlIGN1cnJlbnQgcGFnZSBVUkwgYW5kIGNvbnN0cnVjdCB0aGUgcGF0aCB0byB0aGUgbWFya2Rvd24gZmlsZVxyXG4gICAgY29uc3QgY3VycmVudFBhdGggPSB3aW5kb3cubG9jYXRpb24ucGF0aG5hbWU7XHJcbiAgICBjb25zdCBtYXJrZG93blBhdGggPSBjdXJyZW50UGF0aC5lbmRzV2l0aCgnLycpXHJcbiAgICAgID8gY3VycmVudFBhdGggKyAnaW5kZXgubWQnXHJcbiAgICAgIDogY3VycmVudFBhdGggKyAnL2luZGV4Lm1kJztcclxuXHJcbiAgICAvLyBVcGRhdGUgYnV0dG9uIHRvIHNob3cgbG9hZGluZyBzdGF0ZVxyXG4gICAgYnV0dG9uLmlubmVySFRNTCA9XHJcbiAgICAgICc8c3ZnIHhtbG5zPVwiaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmdcIiB3aWR0aD1cIjE2XCIgaGVpZ2h0PVwiMTZcIiB2aWV3Qm94PVwiMCAwIDI0IDI0XCIgZmlsbD1cIm5vbmVcIiBzdHJva2U9XCJjdXJyZW50Q29sb3JcIiBzdHJva2Utd2lkdGg9XCIyXCIgc3Ryb2tlLWxpbmVjYXA9XCJyb3VuZFwiIHN0cm9rZS1saW5lam9pbj1cInJvdW5kXCIgY2xhc3M9XCJtYi0xIG1lLTFcIj48cGF0aCBzdHJva2U9XCJub25lXCIgZD1cIk0wIDBoMjR2MjRIMHpcIiBmaWxsPVwibm9uZVwiLz48cGF0aCBkPVwiTTEyIDZsMCAtM1wiIC8+PHBhdGggZD1cIk0xNi4yNSA3Ljc1bDIuMTUgLTIuMTVcIiAvPjxwYXRoIGQ9XCJNMTggMTJsMyAwXCIgLz48cGF0aCBkPVwiTTE2LjI1IDE2LjI1bDIuMTUgMi4xNVwiIC8+PHBhdGggZD1cIk0xMiAxOGwwIDNcIiAvPjxwYXRoIGQ9XCJNNy43NSAxNi4yNWwtMi4xNSAyLjE1XCIgLz48cGF0aCBkPVwiTTYgMTJsLTMgMFwiIC8+PHBhdGggZD1cIk03Ljc1IDcuNzVsLTIuMTUgLTIuMTVcIiAvPjwvc3ZnPiBMb2FkaW5nLi4uJztcclxuXHJcbiAgICAvLyBGZXRjaCB0aGUgbWFya2Rvd24gZmlsZSBmcm9tIHRoZSBwdWJsaWMgZGlyZWN0b3J5XHJcbiAgICBmZXRjaChtYXJrZG93blBhdGgpXHJcbiAgICAgIC50aGVuKChyZXNwb25zZSkgPT4ge1xyXG4gICAgICAgIGlmICghcmVzcG9uc2Uub2spIHtcclxuICAgICAgICAgIHRocm93IG5ldyBFcnJvcihgSFRUUCBlcnJvciEgc3RhdHVzOiAke3Jlc3BvbnNlLnN0YXR1c31gKTtcclxuICAgICAgICB9XHJcbiAgICAgICAgcmV0dXJuIHJlc3BvbnNlLnRleHQoKTtcclxuICAgICAgfSlcclxuICAgICAgLnRoZW4oKG1hcmtkb3duKSA9PiB7XHJcbiAgICAgICAgcmV0dXJuIG5hdmlnYXRvci5jbGlwYm9hcmQud3JpdGVUZXh0KG1hcmtkb3duKTtcclxuICAgICAgfSlcclxuICAgICAgLnRoZW4oKCkgPT4ge1xyXG4gICAgICAgIGJ1dHRvbi5pbm5lckhUTUwgPVxyXG4gICAgICAgICAgJzxzdmcgeG1sbnM9XCJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2Z1wiIHdpZHRoPVwiMTZcIiBoZWlnaHQ9XCIxNlwiIHZpZXdCb3g9XCIwIDAgMjQgMjRcIiBmaWxsPVwibm9uZVwiIHN0cm9rZT1cImN1cnJlbnRDb2xvclwiIHN0cm9rZS13aWR0aD1cIjJcIiBzdHJva2UtbGluZWNhcD1cInJvdW5kXCIgc3Ryb2tlLWxpbmVqb2luPVwicm91bmRcIiBjbGFzcz1cIm1iLTEgbWUtMVwiPjxwYXRoIHN0cm9rZT1cIm5vbmVcIiBkPVwiTTAgMGgyNHYyNEgwelwiIGZpbGw9XCJub25lXCIvPjxwYXRoIGQ9XCJNNSAxMmw1IDVsMTAgLTEwXCIgLz48L3N2Zz4gQ29waWVkISc7XHJcbiAgICAgICAgc2V0VGltZW91dCgoKSA9PiB7XHJcbiAgICAgICAgICBidXR0b24uaW5uZXJIVE1MID1cclxuICAgICAgICAgICAgJzxzdmcgeG1sbnM9XCJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2Z1wiIHdpZHRoPVwiMTZcIiBoZWlnaHQ9XCIxNlwiIHZpZXdCb3g9XCIwIDAgMjQgMjRcIiBmaWxsPVwibm9uZVwiIHN0cm9rZT1cImN1cnJlbnRDb2xvclwiIHN0cm9rZS13aWR0aD1cIjJcIiBzdHJva2UtbGluZWNhcD1cInJvdW5kXCIgc3Ryb2tlLWxpbmVqb2luPVwicm91bmRcIiBjbGFzcz1cIm1iLTEgbWUtMVwiPjxwYXRoIHN0cm9rZT1cIm5vbmVcIiBkPVwiTTAgMGgyNHYyNEgwelwiIGZpbGw9XCJub25lXCIvPjxwYXRoIGQ9XCJNNyA5LjY2N2EyLjY2NyAyLjY2NyAwIDAgMSAyLjY2NyAtMi42NjdoOC42NjZhMi42NjcgMi42NjcgMCAwIDEgMi42NjcgMi42Njd2OC42NjZhMi42NjcgMi42NjcgMCAwIDEgLTIuNjY3IDIuNjY3aC04LjY2NmEyLjY2NyAyLjY2NyAwIDAgMSAtMi42NjcgLTIuNjY3bDAgLTguNjY2XCIgLz48cGF0aCBkPVwiTTQuMDEyIDE2LjczN2EyLjAwNSAyLjAwNSAwIDAgMSAtMS4wMTIgLTEuNzM3di0xMGMwIC0xLjEgLjkgLTIgMiAtMmgxMGMuNzUgMCAxLjE1OCAuMzg1IDEuNSAxXCIgLz48L3N2Zz4gQ29weSBNYXJrZG93bic7XHJcbiAgICAgICAgfSwgMjAwMCk7XHJcbiAgICAgIH0pXHJcbiAgICAgIC5jYXRjaCgoZXJyKSA9PiB7XHJcbiAgICAgICAgYnV0dG9uLmlubmVySFRNTCA9XHJcbiAgICAgICAgICAnPHN2ZyB4bWxucz1cImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnXCIgd2lkdGg9XCIxNlwiIGhlaWdodD1cIjE2XCIgdmlld0JveD1cIjAgMCAyNCAyNFwiIGZpbGw9XCJub25lXCIgc3Ryb2tlPVwiY3VycmVudENvbG9yXCIgc3Ryb2tlLXdpZHRoPVwiMlwiIHN0cm9rZS1saW5lY2FwPVwicm91bmRcIiBzdHJva2UtbGluZWpvaW49XCJyb3VuZFwiIGNsYXNzPVwibWItMSBtZS0xXCI+PHBhdGggc3Ryb2tlPVwibm9uZVwiIGQ9XCJNMCAwaDI0djI0SDB6XCIgZmlsbD1cIm5vbmVcIi8+PHBhdGggZD1cIk0xOCA2bC0xMiAxMlwiIC8+PHBhdGggZD1cIk02IDZsMTIgMTJcIiAvPjwvc3ZnPiBFcnJvcic7XHJcbiAgICAgICAgY29uc29sZS5lcnJvcignRmFpbGVkIHRvIGNvcHkgbWFya2Rvd246JywgZXJyKTtcclxuXHJcbiAgICAgICAgLy8gUmVzZXQgYnV0dG9uIGFmdGVyIGEgZGVsYXlcclxuICAgICAgICBzZXRUaW1lb3V0KCgpID0+IHtcclxuICAgICAgICAgIGJ1dHRvbi5pbm5lckhUTUwgPVxyXG4gICAgICAgICAgICAnPHN2ZyB4bWxucz1cImh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnXCIgd2lkdGg9XCIxNlwiIGhlaWdodD1cIjE2XCIgdmlld0JveD1cIjAgMCAyNCAyNFwiIGZpbGw9XCJub25lXCIgc3Ryb2tlPVwiY3VycmVudENvbG9yXCIgc3Ryb2tlLXdpZHRoPVwiMlwiIHN0cm9rZS1saW5lY2FwPVwicm91bmRcIiBzdHJva2UtbGluZWpvaW49XCJyb3VuZFwiIGNsYXNzPVwibWItMSBtZS0xXCI+PHBhdGggc3Ryb2tlPVwibm9uZVwiIGQ9XCJNMCAwaDI0djI0SDB6XCIgZmlsbD1cIm5vbmVcIi8+PHBhdGggZD1cIk03IDkuNjY3YTIuNjY3IDIuNjY3IDAgMCAxIDIuNjY3IC0yLjY2N2g4LjY2NmEyLjY2NyAyLjY2NyAwIDAgMSAyLjY2NyAyLjY2N3Y4LjY2NmEyLjY2NyAyLjY2NyAwIDAgMSAtMi42NjcgMi42NjdoLTguNjY2YTIuNjY3IDIuNjY3IDAgMCAxIC0yLjY2NyAtMi42NjdsMCAtOC42NjZcIiAvPjxwYXRoIGQ9XCJNNC4wMTIgMTYuNzM3YTIuMDA1IDIuMDA1IDAgMCAxIC0xLjAxMiAtMS43Mzd2LTEwYzAgLTEuMSAuOSAtMiAyIC0yaDEwYy43NSAwIDEuMTU4IC4zODUgMS41IDFcIiAvPjwvc3ZnPiBDb3B5IE1hcmtkb3duJztcclxuICAgICAgICB9LCAyMDAwKTtcclxuICAgICAgfSk7XHJcbiAgfSk7XHJcbn1cclxuIiwgIi8vIFNvdXJjZTogaHR0cHM6Ly9naXRodWIuY29tL2VsZWN0ZXJpb3VzL2Jhc2ljTGlnaHRib3gvaXNzdWVzLzEwI2lzc3VlY29tbWVudC0zNDY4OTgxNDZcclxuaW1wb3J0ICogYXMgYmFzaWNMaWdodGJveCBmcm9tICdiYXNpY2xpZ2h0Ym94JztcclxuXHJcbmNvbnN0IGltYWdlcyA9IGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoJy5saWdodGJveCBpbWcsIGltZy5saWdodGJveCcpO1xyXG5cclxuaW1hZ2VzLmZvckVhY2goKGltZykgPT4ge1xyXG4gIGNvbnN0IGh0bWwgPSBpbWcub3V0ZXJIVE1MO1xyXG5cclxuICAvLyBGdW5jdGlvbiB0byBoYW5kbGUgZXNjYXBlIGtleVxyXG4gIGNvbnN0IGhhbmRsZUVzY2FwZUtleSA9IChldmVudCkgPT4ge1xyXG4gICAgaWYgKGV2ZW50LmtleSA9PT0gJ0VzY2FwZScpIHtcclxuICAgICAgaW5zdGFuY2UuY2xvc2UoKTtcclxuICAgIH1cclxuICB9O1xyXG5cclxuICBjb25zdCBpbnN0YW5jZSA9IGJhc2ljTGlnaHRib3guY3JlYXRlKGh0bWwsIHtcclxuICAgIG9uU2hvdzogKCkgPT4ge1xyXG4gICAgICAvLyBBZGQgZXNjYXBlIGtleSBsaXN0ZW5lciB3aGVuIGxpZ2h0Ym94IGlzIHNob3duXHJcbiAgICAgIGRvY3VtZW50LmFkZEV2ZW50TGlzdGVuZXIoJ2tleWRvd24nLCBoYW5kbGVFc2NhcGVLZXkpO1xyXG4gICAgfSxcclxuICAgIG9uQ2xvc2U6ICgpID0+IHtcclxuICAgICAgLy8gUmVtb3ZlIGVzY2FwZSBrZXkgbGlzdGVuZXIgd2hlbiBsaWdodGJveCBpcyBjbG9zZWRcclxuICAgICAgZG9jdW1lbnQucmVtb3ZlRXZlbnRMaXN0ZW5lcigna2V5ZG93bicsIGhhbmRsZUVzY2FwZUtleSk7XHJcbiAgICB9LFxyXG4gIH0pO1xyXG5cclxuICBpbWcub25jbGljayA9ICgpID0+IHtcclxuICAgIGluc3RhbmNlLnNob3coKTtcclxuICB9O1xyXG59KTtcclxuIl0sCiAgIm1hcHBpbmdzIjogIjs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7Ozs7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LFVBQVU7QUFDMUIsaUJBQVMsTUFBTTtBQUFBLE1BQ2pCO0FBQUEsSUFDRjtBQUVBLFVBQU0sV0FBeUIscUJBQU8sTUFBTTtBQUFBLE1BQzFDLFFBQVEsTUFBTTtBQUVaLGlCQUFTLGlCQUFpQixXQUFXLGVBQWU7QUFBQSxNQUN0RDtBQUFBLE1BQ0EsU0FBUyxNQUFNO0FBRWIsaUJBQVMsb0JBQW9CLFdBQVcsZUFBZTtBQUFBLE1BQ3pEO0FBQUEsSUFDRixDQUFDO0FBRUQsUUFBSSxVQUFVLE1BQU07QUFDbEIsZUFBUyxLQUFLO0FBQUEsSUFDaEI7QUFBQSxFQUNGLENBQUM7IiwKICAibmFtZXMiOiBbIndpbmRvdyIsICJsYXp5U2l6ZXMiLCAibCIsICJkb2N1bWVudCIsICJEYXRlIiwgInNldFRpbWVvdXQiLCAicmVxdWVzdEFuaW1hdGlvbkZyYW1lIiwgImUiLCAiaSIsICJsb2FkTW9kZSIsICJ3aW5kb3ciLCAiZG9jdW1lbnQiLCAibGF6eVNpemVzIiwgImUiLCAiX193ZWJwYWNrX3JlcXVpcmVfXyIsICJDbGlwYm9hcmRBY3Rpb25DdXQiLCAiZmFrZUNvcHlBY3Rpb24iLCAiQ2xpcGJvYXJkQWN0aW9uQ29weSIsICJfdHlwZW9mIiwgIm9iaiIsICJDbGlwYm9hcmRBY3Rpb25EZWZhdWx0IiwgImkiLCAibyIsICJfc2V0UHJvdG90eXBlT2YiLCAicCIsICJzZWxmIiwgImUiLCAiX2dldFByb3RvdHlwZU9mIiwgIkNsaXBib2FyZCIsICJhY3Rpb24iLCAibW9kdWxlIiwgImV4cG9ydHMiLCAiZSIsICJuIiwgInQiLCAibyIsICJyIiwgImMiLCAidSIsICJzIiwgImkiLCAiYSIsICJsIiwgImUiLCAiciIsICJuIiwgInQiLCAibyIsICJsIiwgImYiLCAiaSIsICJ1IiwgImEiLCAiZCIsICJjIiwgInMiLCAibGF6eVNpemVzIiwgImkiLCAiQ2xpcGJvYXJkIiwgImUiLCAiaSIsICJlIiwgImUiXQp9Cg==
