// src/components/SelectedProjects.jsx
import React from "react";
import "./SelectedProjects.css";

export default function SelectedProjects({ projects = [] }) {
  if (projects.length === 0) return <div className="selected-projects-container">No projects selected.</div>;

  return (
    <div className="selected-projects-container">
      <h3>Selected Projects</h3>
      <ul>
        {projects.map((p, idx) => (
          <li key={idx}>
            {p.project_name || p.ProjectName || p.project} {p.cost ? `($${p.cost})` : ""}
          </li>
        ))}
      </ul>
    </div>
  );
}
