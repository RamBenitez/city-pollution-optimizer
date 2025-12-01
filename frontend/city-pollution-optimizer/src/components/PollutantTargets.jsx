// src/components/PollutantTargets.jsx
import React from "react";
import "./PollutantTargets.css";

export default function PollutantTargets({ pollutants = [] }) {
  return (
    <div className="pollutant-targets-container">
      <h3>Pollutant Targets</h3>
      <table>
        <thead>
          <tr>
            <th>Pollutants</th>
            <th>Target</th>
          </tr>
        </thead>
        <tbody>
          {pollutants.map((p, idx) => (
            <tr key={idx}>
              <td>{p.Pollutant}</td>
              <td>{p.Target}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
