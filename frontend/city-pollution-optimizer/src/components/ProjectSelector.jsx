import React from "react";
import "./ProjectSelector.css";

export default function ProjectSelector({ projects = [], selected = new Set(), onToggle }) {
  return (
    <div className="project-selector">
      <table className="project-table">
        <thead>
          <tr className="project-header">
            <th></th> 
            <th>Project</th>
            <th>Cost</th>
          </tr>
        </thead>
        <tbody>
          {projects.map((p, idx) => {
            const id = idx + 1;
            const label =
              p.project_name || 
              p.ProjectName || 
              p.project || 
              p[1] || 
              p[2] ||
              `Project ${id}`;
            return (
              <tr key={id} className="project-row">
                <td>
                  <input
                    type="checkbox"
                    checked={selected.has(id)}
                    onChange={() => onToggle(id)}
                  />
                </td>
                <td className="project-name">{label}</td>
                <td className="project-cost">{p.cost ? `$${p.cost}` : ""}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}
