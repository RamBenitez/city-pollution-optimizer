import { useState } from 'react'
import {Routes, Route} from 'react-router-dom';
import ProjectsPage from './pages/ProjectsPage';
import LandingPage from './pages/LandingPage';
import './App.css'

function App() {
  return (
    <Routes>
      <Route path="/" element={<LandingPage />} />
      <Route path="/projects" element={<ProjectsPage />} />
    </Routes>
  );
}
export default App
