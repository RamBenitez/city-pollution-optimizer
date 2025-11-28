//ProjectSelector.jsx

import React from "react";
import "./ProjectSelector.css";

export default function ProjectSelector({ projects = [], selected = new Set(), onToggle }) {
  return (
    // container fro the project list for user to select
    <div className="project-selector">
      // loop through every projects
      {projects.map((p, idx) => {
        //gets unique id
        const id = idx + 1;

        // safely extract project name
        // human readable project name
        const label =
          p.project_name || 
          p.ProjectName || 
          p.project || 
          p[1] || 
          p[2] ||
          `Project ${id}`;

        return (
          <div key={id} className="project-item">
            <label>
              <input
                type="checkbox"
                checked={selected.has(id)}
                onChange={() => onToggle(id)}
              />
              {" "}
              {label} {p.cost ? `($${p.cost})` : ""}
            </label>
          </div>
        );
      })}
    </div>
  );
}
