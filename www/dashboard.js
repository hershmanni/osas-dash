(function () {
  "use strict";

  var sourcePlots = {
    trend_plot: "trend",
    ranking_plot: "ranking",
    grade_heatmap: "rank_heatmap",
    cohort_plot: "cohort"
  };

  var sourceDetailOutputs = {
    trend_source_details: "trend",
    ranking_source_details: "ranking",
    heatmap_source_details: "rank_heatmap",
    cohort_source_details: "cohort"
  };

  var sourceClickInputs = {
    "plotly_click-trend": "trend",
    "plotly_click-ranking": "ranking",
    "plotly_click-rank_heatmap": "rank_heatmap",
    "plotly_click-cohort": "cohort"
  };

  var sourceMarkerState = {};

  function plotForSource(source) {
    var outputId = Object.keys(sourcePlots).find(function (id) {
      return sourcePlots[id] === source;
    });
    return outputId ? document.getElementById(outputId) : null;
  }

  function removeSelectionMarker(plot, source) {
    if (plot) {
      var marker = plot.querySelector(":scope > .plot-source-selection-marker");
      if (marker) marker.remove();
    }
    delete sourceMarkerState[source];
  }

  function renderSelectionMarker(plot, source) {
    var markerState = sourceMarkerState[source];
    if (!plot || !markerState) return;

    var point = findPointByKey(plot, markerState.key);
    if (!point) {
      var staleMarker = plot.querySelector(":scope > .plot-source-selection-marker");
      if (staleMarker) staleMarker.remove();
      return;
    }
    var position = pointPosition(plot, point);
    if (!position) return;

    var marker = plot.querySelector(":scope > .plot-source-selection-marker");
    if (!marker) {
      marker = document.createElement("span");
      marker.className = "plot-source-selection-marker";
      marker.setAttribute("aria-hidden", "true");
      plot.appendChild(marker);
    }
    if (window.getComputedStyle(plot).position === "static") plot.style.position = "relative";
    marker.style.left = position.x + "px";
    marker.style.top = position.y + "px";
  }

  function keyIndex(keys, selectedKey) {
    if (!Array.isArray(keys)) return String(keys) === selectedKey ? 0 : null;
    for (var row = 0; row < keys.length; row += 1) {
      if (Array.isArray(keys[row])) {
        for (var column = 0; column < keys[row].length; column += 1) {
          if (String(keys[row][column]) === selectedKey) return [row, column];
        }
      } else if (String(keys[row]) === selectedKey) {
        return row;
      }
    }
    return null;
  }

  function findPointByKey(plot, selectedKey) {
    var traces = plot.data || [];
    var prioritizedTraces = traces.map(function (trace, curveNumber) {
      var hasMarker = String(trace.mode || "").indexOf("markers") !== -1;
      return {
        trace: trace,
        curveNumber: curveNumber,
        priority: trace.type === "heatmap" || hasMarker ? 0 : 1
      };
    }).sort(function (left, right) {
      return left.priority - right.priority || left.curveNumber - right.curveNumber;
    });

    for (var index = 0; index < prioritizedTraces.length; index += 1) {
      var trace = prioritizedTraces[index].trace;
      if (trace.key === undefined || trace.key === null) continue;
      var pointNumber = keyIndex(trace.key, selectedKey);
      if (pointNumber !== null) {
        return { trace: trace, pointNumber: pointNumber };
      }
    }
    return null;
  }

  function pointValue(values, pointNumber, dimension) {
    if (!Array.isArray(values)) return values;
    if (Array.isArray(pointNumber)) {
      return dimension === "x" ? values[pointNumber[1]] : values[pointNumber[0]];
    }
    return values[pointNumber];
  }

  function layoutAxisName(axisReference, dimension) {
    var reference = axisReference || dimension;
    return dimension + "axis" + reference.slice(1);
  }

  function axisPixel(plot, trace, dimension, value) {
    var axis = plot._fullLayout && plot._fullLayout[layoutAxisName(trace[dimension + "axis"], dimension)];
    if (!axis) return null;
    var relativePixel = typeof axis.d2p === "function" ? axis.d2p(value) : axis.l2p(value);
    return Number.isFinite(relativePixel) ? axis._offset + relativePixel : null;
  }

  function pointPosition(plot, point) {
    var xValue = pointValue(point.trace.x, point.pointNumber, "x");
    var yValue = pointValue(point.trace.y, point.pointNumber, "y");
    var x = axisPixel(plot, point.trace, "x", xValue);
    var y = axisPixel(plot, point.trace, "y", yValue);
    return x === null || y === null ? null : { x: x, y: y };
  }

  function pointKey(point) {
    if (!point) return null;
    if (point.key !== undefined && point.key !== null) return String(point.key);
    if (point.id !== undefined && point.id !== null) return String(point.id);
    var keys = point.data && point.data.key;
    if (Array.isArray(keys) && typeof point.pointNumber === "number") {
      if (keys[point.pointNumber] !== undefined) return String(keys[point.pointNumber]);
    }
    if (Array.isArray(keys) && Array.isArray(point.pointNumber)) {
      var keyRow = keys[point.pointNumber[0]];
      if (Array.isArray(keyRow) && keyRow[point.pointNumber[1]] !== undefined) {
        return String(keyRow[point.pointNumber[1]]);
      }
    }
    return String(point.curveNumber) + ":" + JSON.stringify(point.pointNumber);
  }

  function updateSelectionMarker(plot, source, point) {
    var key = pointKey(point);
    if (!key) return;

    if (sourceMarkerState[source] && sourceMarkerState[source].key === key) {
      removeSelectionMarker(plot, source);
      return;
    }

    sourceMarkerState[source] = { key: key };
    renderSelectionMarker(plot, source);
  }

  function pointFromShinyValue(value) {
    var parsed = value;
    if (typeof parsed === "string") {
      try {
        parsed = JSON.parse(parsed);
      } catch (error) {
        return null;
      }
    }
    if (Array.isArray(parsed)) return parsed[0] || null;
    return parsed && typeof parsed === "object" ? parsed : null;
  }

  function handlePointInput(source, value) {
    var plot = plotForSource(source);
    if (!plot) return;
    window.clearTimeout(plot.__sourceBlankClearTimer);
    plot.__sourceBlankClearTimer = null;
    plot.__sourceLastPointInputAt = window.performance.now();
    updateSelectionMarker(plot, source, pointFromShinyValue(value));
  }

  function clearSourceSelection(source) {
    removeSelectionMarker(plotForSource(source), source);
    if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") return;
    window.Shiny.setInputValue("clear_source_" + source, Date.now(), { priority: "event" });
  }

  window.clearDashboardSourceSelection = clearSourceSelection;

  function syncMarkerToDetails(outputId, source) {
    var details = document.getElementById(outputId);
    if (details && details.querySelector(".plot-source-placeholder")) {
      removeSelectionMarker(plotForSource(source), source);
    }
  }

  function bindBlankPlotClick(outputId, source) {
    var plot = document.getElementById(outputId);
    if (!plot || typeof plot.on !== "function") return;

    if (plot.dataset.sourceClearBound !== source) {
      plot.dataset.sourceClearBound = source;

      plot.addEventListener("pointerdown", function (event) {
        plot.__sourcePointerDown = { x: event.clientX, y: event.clientY };
      }, true);

      plot.addEventListener("click", function (event) {
        if (event.target.closest(
          ".modebar, .legend, .rangeslider-container, .slider-container, .updatemenu-container, " +
          ".scatterlayer .trace, .barlayer .point, .heatmaplayer .hm, .heatmaplayer image"
        )) {
          return;
        }

        var pointerDown = plot.__sourcePointerDown;
        if (pointerDown && Math.hypot(event.clientX - pointerDown.x, event.clientY - pointerDown.y) > 6) {
          return;
        }

        if (!event.target.closest(".nsewdrag, .drag, .bg, .plotbg")) return;

        window.clearTimeout(plot.__sourceBlankClearTimer);
        plot.__sourceBlankClearTimer = window.setTimeout(function () {
          var elapsedSincePoint = window.performance.now() - (plot.__sourceLastPointInputAt || 0);
          if (elapsedSincePoint > 250) clearSourceSelection(source);
        }, 200);
      }, true);
    }

    if (plot.__sourceAfterPlotHandler && typeof plot.removeListener === "function") {
      plot.removeListener("plotly_afterplot", plot.__sourceAfterPlotHandler);
    }
    plot.__sourceAfterPlotHandler = function () {
      renderSelectionMarker(plot, source);
    };
    plot.on("plotly_afterplot", plot.__sourceAfterPlotHandler);
    renderSelectionMarker(plot, source);
  }

  function bindAllSourcePlots() {
    Object.keys(sourcePlots).forEach(function (outputId) {
      bindBlankPlotClick(outputId, sourcePlots[outputId]);
    });
  }

  function syncAllDetailMarkers() {
    Object.keys(sourceDetailOutputs).forEach(function (outputId) {
      syncMarkerToDetails(outputId, sourceDetailOutputs[outputId]);
    });
  }

  document.addEventListener("DOMContentLoaded", bindAllSourcePlots);
  if (window.jQuery) {
    window.jQuery(document)
      .on("shiny:connected.dashboardSourceClear", bindAllSourcePlots)
      .on("shiny:idle.dashboardSourceClear", function () {
        bindAllSourcePlots();
        syncAllDetailMarkers();
      })
      .on("shiny:inputchanged.dashboardSourceClear", function (event) {
        if (sourceClickInputs[event.name]) {
          handlePointInput(sourceClickInputs[event.name], event.value);
        }
      })
      .on("shiny:value.dashboardSourceClear", function (event) {
        if (sourcePlots[event.name]) window.requestAnimationFrame(bindAllSourcePlots);
        if (sourceDetailOutputs[event.name]) {
          window.requestAnimationFrame(function () {
            syncMarkerToDetails(event.name, sourceDetailOutputs[event.name]);
          });
        }
      });
  }
}());
