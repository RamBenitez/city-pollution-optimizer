import React, { useEffect, useState } from "react";
import ProjectSelector from "../components/ProjectSelector";
import ResultsTable from "../components/ResultsTable";
import TableauViewer from "../components/TableauViewer";
import PollutantTargets from "../components/PollutantTargets";
import SelectedProjects from "../components/SelectedProjects";

import "./SolverPage.css";
import logoImg from "../assets/logo2.png"; 

//backend api
const API_BASE = "http://localhost:8000";

export default function SolverPage() {
  //react states used
  const [projects, setProjects] = useState([]); // list of mitigation projects
  const [pollutants, setPollutants] = useState([]); // pollutant types from backend
  const [selected, setSelected] = useState(new Set()); // which projects the user selected
  const [loading, setLoading] = useState(false); // loading indicator when solving
  const [result, setResult] = useState(null); // LP solution result
  const [trace, setTrace] = useState(null); // tableau trace (optional)
  const [error, setError] = useState(null); // error messages

  const [minUnits, setMinUnits] = useState(0); // lower bound for units of each project
  const [maxUnits, setMaxUnits] = useState(20); // upper bound for units

  useEffect(() => {
    // Fetch list of projects from the backend
    fetch(`${API_BASE}/projects`)
      .then((r) => r.json())
      .then(setProjects)
      .catch((e) => setError("Failed to load projects: " + e.message));
    // Fetch pollutant list (for target table)
    fetch(`${API_BASE}/pollutants`)
      .then((r) => r.json())
      .then(setPollutants)
      .catch((e) => setError("Failed to load pollutants: " + e.message));
  }, []);

   // Toggles one project
  function toggleProject(id) {
    const s = new Set(selected);
    if (s.has(id)) s.delete(id);
    else s.add(id);
    setSelected(s);
  }
  // Select all projects at once
  function selectAll() {
    setSelected(new Set(projects.map((_, i) => i + 1)));
  }

  // Reset all selections + clear results
  function resetAll() {
    setSelected(new Set());
    setResult(null);
    setTrace(null);
    setError(null);
  }


  async function solve(return_trace = true) {
      if (selected.size === 0) {
    setError("Nothing was selected. Please select at least one project.");
    return;
  }
    setLoading(true);
    setError(null);
    setResult(null);
    setTrace(null);

    try {
      // POST request to backend solver endpoint
      const res = await fetch(`${API_BASE}/solve`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          selected_projects: Array.from(selected),
          return_trace,
          lower_bound: minUnits,
          upper_bound: maxUnits
        }),
      });
      // If backend returns error, throw it
      if (!res.ok) throw new Error(await res.text());
      const json = await res.json();
      if (typeof json.solution === "string") json.solution = JSON.parse(json.solution);
      if (typeof json.tableau_trace === "string") json.tableau_trace = JSON.parse(json.tableau_trace);
      //infeasible
      if (json.feasible === false) setError(json.message || "Infeasible LP");
      else {
        setResult(json);
        setTrace(json.tableau_trace || []);
      }
    } catch (e) {
      setError("Solve failed: " + e.message);
    } finally {
      setLoading(false);
    }
  }

  const achievements = (() => {
    if (!result || !result.solution || !projects.length) return null;
    const solution = result.solution;
    const achieved = {};
    pollutants.forEach((p) => { achieved[p.Pollutant] = 0; });
    solution.forEach((item) => {
      const project = projects[item.project_index - 1];
      pollutants.forEach((pol) => {
        achieved[pol.Pollutant] += Number(project[pol.Pollutant]) * Number(item.units);
      });
    });
    return achieved;
  })();

  //render layout of solverpage
  return (
    <div className="solver-container">
      {/* Header with logo + title */}
      <div className="solver-header">
        <img src={logoImg} alt="City Logo" className="solver-logo" />
        <span className="solver-header-title">GreenVale City Pollution Reduction Solver</span>
      </div>

      {error && <div className="error-box">{String(error)}</div>}

      <div className="solver-grid">
        {/* LEFT PANEL */}
        <div className="left-panel">
          <h2>Mitigation Projects</h2>
          <div className="button-row">
            <button onClick={selectAll}>Select All</button>
            <button onClick={resetAll}>Reset</button>
          </div>

          <div className="bounds-row">
            <label>
              Min Units:
              <input
                type="number"
                value={minUnits}
                onChange={(e) => setMinUnits(Number(e.target.value))}
              />
            </label>
            <label>
              Max Units:
              <input
                type="number"
                value={maxUnits}
                onChange={(e) => setMaxUnits(Number(e.target.value))}
              />
            </label>
          </div>
          {/* Selectable list of projects */}
          <ProjectSelector
            projects={projects}
            selected={selected}
            onToggle={toggleProject}
          />
          {/* Solve button */}
          <button
            className="solve-btn"
            onClick={() => solve(true)}
            disabled={loading}
          >
            {loading ? "Solving..." : "Solve Optimization"}
          </button>
        </div>

        {/* MIDDLE PANEL */}
        <div className="middle-panel">
          <PollutantTargets pollutants={pollutants} />
          <SelectedProjects
            projects={Array.from(selected).map((id) => projects[id - 1])}
          />
        </div>
        {/* RIGHT PANEL */}
        <div className="right-panel">
          <div className="results-section">
            <h2>Final Solution</h2>
            {result && (
              <ResultsTable
                data={result.solution}
                total={result.total_cost}
              />
            )}
          </div>

          <div className="tableau-section">
            <h2>Tableau</h2>

            {trace && trace.length > 0 ? (
              <div className="tableau-scroll">
                <TableauViewer trace={trace} />
              </div>
            ) : (
              <p>No tableau available.</p>
            )}
          </div>
        </div>

      </div>
    </div>
  );
}
