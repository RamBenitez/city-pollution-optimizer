import { useNavigate } from "react-router-dom";
import "./LandingPage.css";
import logoImg from "../assets/logo2.png";   
import planGif from "../assets/logo1.gif";   

function LandingPage() {
  const navigate = useNavigate();

  const gotoProjects = () => {
    navigate("/projects");
  };

  return (
    <div className="landing-container">
      {/* Header */}
      <header className="landing-header">
        <div className="logo-title">
          <img src={logoImg} alt="Greenvale City Logo" className="city-logo" />
          <h1 className="header-title">Pollution Reduction Solver</h1>
        </div>
        <nav className="landing-nav">
          <span>Home</span>
          <span>About Greenvale</span>
          <span>Pollution Campaigns</span>
          <span>Contacts</span>
        </nav>
      </header>

      {/* Main content */}
      <div className="landing-main">
        <img src={planGif} alt="Pollution Reduction Plan" className="plan-gif" />
        <button className="landing-button" onClick={gotoProjects}>
          Pollution Solver
        </button>
      </div>
    </div>
  );
}

export default LandingPage;
