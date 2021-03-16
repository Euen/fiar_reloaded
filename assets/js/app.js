// We need to import the CSS so that webpack will load it.
// The MiniCssExtractPlugin is used to separate it out into
// its own CSS file.
import "../css/app.scss";

// webpack automatically bundles all modules in your
// entry points. Those entry points can be configured
// in "webpack.config.js".
//
// Import deps with the dep name or local files with a relative path, for example:
//
//     import {Socket} from "phoenix"
//     import socket from "./socket"
//
import "phoenix_html";
import { Socket } from "phoenix";
import NProgress from "nprogress";
import { LiveSocket, DOM } from "phoenix_live_view";

let csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

//   let liveSocket = new LiveSocket("/live", Socket, {
//   params: { _csrf_token: csrfToken },
//   metadata: {
//     keyup: (e, el) => {
//       return {
//         key: e.key,
//         column: circle.parentElement.classList[1]
//       }
//     }
//   }
// });

// hooks.DropChip = {
//   mounted() {
//     window.addEventListener("ArrowDownEvent", (e) => {
//       console.log("pepe");
//       let class_names = circle.parentElement.classList
//       this.pushEvent("drop_chip", { "column": class_names }, (reply, ref) =>
//         // do the animation here having in count the reply
//         console.log(reply)
//       );
//     });

//     // window.addEventListener("keyup", (event) => {
//     //   console.log("KEY up");
//     //   if (event.key == "ArrowDown") {
//     //     let class_names = circle.parentElement.classList
//     //     this.pushEvent("drop_chip", {"column": class_names}, (reply, ref) =>
//     //       // do the animation here having in count the reply
//     //       console.log(reply)
//     //     )
//     //   }
//     // });

//     // this.handleEvent("phxEventToJS", (payload) => console.log("data received: " + payload));
//   },
// };

let liveSocket = new LiveSocket("/live", Socket, {
  params: { _csrf_token: csrfToken },
  metadata: {
    keyup: (e, el) => {
      return {
        key: e.key,
        column: circle.parentElement.classList[1],
      };
    },
  },
  // dom: {
  //   onBeforeElUpdated(from, to) {
  //     console.log(from);
  //     console.log(to);
  //   },
  // },
});

// Show progress bar on live navigation and form submits
window.addEventListener("phx:page-loading-start", (info) => NProgress.start());
window.addEventListener("phx:page-loading-stop", (info) => NProgress.done());

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;

// Functions to move the chip
const arrowDownEvent = new Event("ArrowDownEvent");
window.addEventListener("keyup", (event) => {
  switch (event.key) {
    case "ArrowRight":
      moveRight();
      break;
    case "ArrowDown":
      console.log("DOWN");
      break;
    case "ArrowLeft":
      moveLeft();
      break;
    default:
      break;
  }
});

function moveRight() {
  if (!circle.parentElement.classList.contains("c7")) {
    circle.classList.toggle("animate-right");
    circle.addEventListener(transitionEvent, transitionEndCallback);
  }
}

function moveLeft() {
  if (!circle.parentElement.classList.contains("c1")) {
    circle.classList.toggle("animate-left");
    circle.addEventListener(transitionEvent, transitionEndCallback);
  }
}

// Create event listener to react when the css animation finish
let whichTransitionEvent = () => {
  let t,
    el = document.createElement("fakeelement");

  let transitions = {
    transition: "transitionend",
    OTransition: "oTransitionEnd",
    MozTransition: "transitionend",
    WebkitTransition: "webkitTransitionEnd",
  };

  for (t in transitions) {
    if (el.style[t] !== undefined) {
      return transitions[t];
    }
  }
};

let transitionEvent = whichTransitionEvent();
let circle = document.getElementById("circle");

// Callback called when the animation finish
let transitionEndCallback = (e) => {
  let className = get_animate_class(e.target);
  console.log(className);
  circle.removeEventListener(transitionEvent, transitionEndCallback);
  circle.classList.remove(className);
  switch (className) {
    case "animate-right":
      e.target.parentElement.nextElementSibling.appendChild(circle);
      break;
    case "animate-left":
      e.target.parentElement.previousElementSibling.appendChild(circle);
      break;
    default:
      break;
  }
};

function get_animate_class(element) {
  if (element.classList.contains("animate-right")) {
    return "animate-right";
  }
  if (element.classList.contains("animate-left")) {
    return "animate-left";
  }
  return false;
}
