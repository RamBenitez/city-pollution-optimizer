// src/components/TableauViewer.jsx
import React from "react";
import "./TableauViewer.css";

function SingleTable({ t, idx }) {
  const headers = t.headers || [];
  const rows = t.rows || [];
  const rownames = t.rownames || [];

  return (
    <div className="tableau-table">
      <h4>Iteration {idx + 1}</h4>

      <table>
        <thead>
          <tr>
            <th></th>
            {headers.map((h, i) => (
              <th key={i}>{h}</th>
            ))}
          </tr>
        </thead>

        <tbody>
          {rows.map((r, i) => (
            <tr key={i}>
              <td className="rowname">{rownames[i] || i + 1}</td>

              {r.map((c, j) => (
                <td key={j}>{Number(c).toFixed(6)}</td>
              ))}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

export default function TableauViewer({ trace }) {
  let table_list = trace;

  // If backend passed { tableaux: [...] }
  if (trace && trace.tableaux) {
    table_list = trace.tableaux;
  }

  // If something goes wrong
  if (!Array.isArray(table_list)) {
    try {
      table_list = JSON.parse(trace);
    } catch {
      return <div>Could not parse tableau trace</div>;
    }
  }

  return (
    <div>
      {table_list.map((t, i) => (
        <SingleTable key={i} t={t} idx={i} />
      ))}
    </div>
  );
}
