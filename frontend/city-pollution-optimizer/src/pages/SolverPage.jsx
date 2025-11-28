// src/pages/SolverPage.jsx
import React, { useEffect, useState } from "react";
import ProjectSelector from "../components/ProjectSelector";
import ResultsTable from "../components/ResultsTable";
import TableauViewer from "../components/TableauViewer";

import "./SolverPage.css";

const API_BASE = "http://localhost:8000";

export default function SolverPage() {
  const [projects, setProjects] = useState([]);
  const [pollutants, setPollutants] = useState([]);
  const [selected, setSelected] = useState(new Set());
  const [loading, setLoading] = useState(false);
  const [result, setResult] = useState(null);
  const [trace, setTrace] = useState(null);
  const [error, setError] = useState(null);

  // Load backend data
  useEffect(() => {
    fetch(`${API_BASE}/projects`)
      .then((r) => r.json())
      .then(setProjects)
      .catch((e) => setError("Failed to load projects: " + e.message));

    fetch(`${API_BASE}/pollutants`)
      .then((r) => r.json())
      .then(setPollutants)
      .catch((e) => setError("Failed to load pollutants: " + e.message));
  }, []);

  // Checkbox toggling
  function toggleProject(id) {
    const s = new Set(selected);
    if (s.has(id)) s.delete(id);
    else s.add(id);
    setSelected(s);
  }

  function selectAll() {
    setSelected(new Set(projects.map((_, i) => i + 1)));
  }

  function resetAll() {
    setSelected(new Set());
    setResult(null);
    setTrace(null);
    setError(null);
  }

  // Solve LP
  async function solve(return_trace = true) {
    setLoading(true);
    setError(null);
    setResult(null);
    setTrace(null);

    try {
      const res = await fetch(`${API_BASE}/solve`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          selected_projects: Array.from(selected),
          return_trace,
        }),
      });

      if (!res.ok) throw new Error(await res.text());
      const json = await res.json();

      if (json.feasible === false) {
        setError(json.message || "Infeasible LP");
      } else {
        setResult(json);
        setTrace(json.tableau_trace || []);
      }
    } catch (e) {
      setError("Solve failed: " + e.message);
    } finally {
      setLoading(false);
    }
  }

  return (
    <div className="solver-container">
      <h1 className="solver-title">GreenVale City Pollution Reduction Plan — Solver</h1>

      {error && <div className="error-box">{String(error)}</div>}

      <div className="solver-grid">

        {/* LEFT PANEL */}
        <div className="left-panel">
          <h2>Mitigation Projects</h2>

          <div className="button-row">
            <button onClick={selectAll}>Select All</button>
            <button onClick={resetAll}>Reset</button>
          </div>

          <ProjectSelector
            projects={projects}
            selected={selected}
            onToggle={toggleProject}
          />

          <button
            className="solve-btn"
            onClick={() => solve(true)}
            disabled={loading}
          >
            {loading ? "Solving..." : "Solve Optimization"}
          </button>
        </div>

        {/* RIGHT PANEL */}
        <div className="right-panel">

          {/* TOP RIGHT — TABLEAU TRACE */}
          <div className="tableau-section">
            <h2>Simplex Tableau Trace</h2>
            {trace ? <TableauViewer trace={trace} /> : <div>No trace yet</div>}
          </div>

          {/* BOTTOM RIGHT — FINAL RESULTS */}
          <div className="results-section">
            <h2>Final Solution</h2>

            <ul>
              {pollutants.map((p, idx) => (
                <li key={idx}>
                  {p.Pollutant}: {p.Target}
                </li>
              ))}
            </ul>

            {result && (
              <ResultsTable data={result.solution} total={result.total_cost} />
            )}
          </div>

        </div>
      </div>
    </div>
  );
}
