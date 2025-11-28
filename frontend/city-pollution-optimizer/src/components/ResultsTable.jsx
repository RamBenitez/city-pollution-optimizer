//ResultsTable.jsx
import React from "react";
import "./ResultsTable.css";

export default function ResultsTable({ data = [], total = null }) {
  return (
    <div className="results-table">
      <table>
        <thead>
          <tr>
            <th>Project</th>
            <th>Units</th>
            <th>Cost/Unit</th>
            <th>Total Cost</th>
          </tr>
        </thead>

        <tbody>
          {data.map((r, i) => (
            <tr key={i}>
              <td>{r.project_name || r.ProjectName || r.project}</td>
              <td>{Number(r.units).toFixed(4)}</td>
              <td>{r.cost_each}</td>
              <td>{Number(r.total_cost).toFixed(2)}</td>
            </tr>
          ))}
        </tbody>
      </table>

      {total !== null && (
        <div className="total-cost">
          Total Cost: ${Number(total).toFixed(2)}
        </div>
      )}
    </div>
  );
}
