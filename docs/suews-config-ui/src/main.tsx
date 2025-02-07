import React from "react";
import ReactDOM from "react-dom/client";
import App from "./App";
import "./index.css";

// Function to initialize the app
function initApp() {
  // Find all editor containers
  const containers = document.getElementsByClassName("suews-config-editor");

  // Mount the app in each container
  Array.from(containers).forEach(container => {
    const root = document.createElement("div");
    root.id = "suews-config-root";
    container.appendChild(root);

    ReactDOM.createRoot(root).render(
      <React.StrictMode>
        <App />
      </React.StrictMode>
    );
  });
}

// Initialize when DOM is ready
if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", initApp);
} else {
  initApp();
}