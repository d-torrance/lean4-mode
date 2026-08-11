// Navigation for the lean4-mode web manual.
//
// SPDX-License-Identifier: Apache-2.0
//
// Two jobs, both optional: highlight the section being read in the table
// of contents, and collapse that table on a screen too narrow to give it
// a sidebar.  The page is entirely usable with this file missing.

(function () {
  "use strict";

  var toc = document.getElementById("table-of-contents");
  var list = document.getElementById("text-table-of-contents");
  if (!toc || !list) {
    return;
  }

  // Where manual.css switches to the fixed sidebar.  Kept in step by hand;
  // the two disagreeing only means the toggle appears alongside the
  // sidebar, which is untidy rather than broken.
  var WIDE = window.matchMedia("(min-width: 64rem)");

  // Collapsing ------------------------------------------------------------

  var heading = toc.querySelector("h2");
  if (heading) {
    var button = document.createElement("button");
    button.id = "toc-toggle";
    button.type = "button";
    button.textContent = heading.textContent;
    button.setAttribute("aria-controls", "text-table-of-contents");
    heading.textContent = "";
    heading.appendChild(button);

    var setCollapsed = function (collapsed) {
      toc.classList.toggle("is-collapsed", collapsed);
      button.setAttribute("aria-expanded", String(!collapsed));
    };

    button.addEventListener("click", function () {
      setCollapsed(!toc.classList.contains("is-collapsed"));
    });

    // Only narrow screens start collapsed, and only they show the toggle:
    // a sidebar that has a whole column to itself has nothing to gain by
    // folding away.
    var fit = function () {
      button.style.display = WIDE.matches ? "none" : "block";
      setCollapsed(!WIDE.matches);
    };

    fit();
    WIDE.addEventListener("change", fit);
  }

  // Current section -------------------------------------------------------

  var links = {};
  var targets = [];

  Array.prototype.forEach.call(list.querySelectorAll("a[href^='#']"), function (link) {
    var id = decodeURIComponent(link.hash.slice(1));
    var heading = document.getElementById(id);
    if (heading) {
      links[id] = link;
      targets.push(heading);
    }
  });

  if (!targets.length) {
    return;
  }

  var current = null;

  // The heading last to cross the top of the viewport is the one being
  // read.  An IntersectionObserver would do as well, but this stays
  // correct when several headings share a screen -- common here, where a
  // subsection can be three lines long.
  var update = function () {
    var found = targets[0];

    for (var i = 0; i < targets.length; i++) {
      if (targets[i].getBoundingClientRect().top > 100) {
        break;
      }
      found = targets[i];
    }

    // Reaching the bottom should light up the last entry even if its
    // heading never makes it past the mark.
    if (window.innerHeight + window.scrollY >= document.body.offsetHeight - 2) {
      found = targets[targets.length - 1];
    }

    var link = links[found.id];
    if (link === current) {
      return;
    }

    if (current) {
      current.classList.remove("is-current");
    }
    link.classList.add("is-current");
    current = link;

    // Keep it in sight in a sidebar long enough to scroll on its own.
    if (WIDE.matches && toc.scrollHeight > toc.clientHeight) {
      var entry = link.getBoundingClientRect();
      var frame = toc.getBoundingClientRect();
      if (entry.top < frame.top || entry.bottom > frame.bottom) {
        toc.scrollTop += entry.top - frame.top - frame.height / 3;
      }
    }
  };

  var pending = false;
  var schedule = function () {
    if (pending) {
      return;
    }
    pending = true;
    window.requestAnimationFrame(function () {
      pending = false;
      update();
    });
  };

  window.addEventListener("scroll", schedule, { passive: true });
  window.addEventListener("resize", schedule);
  update();
})();
