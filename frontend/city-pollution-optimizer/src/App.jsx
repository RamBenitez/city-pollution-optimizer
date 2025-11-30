// App.jsx
import { BrowserRouter as Router, Routes, Route } from "react-router-dom";
import LandingPage from "./pages/landingpage";
import LandingPage from "./pages/LandingPage";
import SolverPage from "./pages/SolverPage";
import "./App.css";

function App() {
  return (
    <router>
      <Routes>
        <Route path="/" element={<LandingPage />} />
        <Route path="/projects" element={<SolverPage />} />
        <Route path="/solver" element={<SolverPage />} />
      </Routes>
    </router>
  );
}

export default App;
