// App.jsx
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import LandingPage from "./pages/LandingPage";
import SolverPage from "./pages/SolverPage";
import "./App.css";

function App() {
  return (
  
      <Routes>
        <Route path="/" element={<LandingPage />} />
        <Route path="/projects" element={<SolverPage />} />
        <Route path="/solver" element={<SolverPage />} />
      </Routes>
  
  );
}

export default App;
