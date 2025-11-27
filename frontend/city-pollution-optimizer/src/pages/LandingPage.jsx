//landingpage

import {useNavigate} from "react-router-dom";
import "./LandingPage.css";

function LandingPage(){
    const navigate = useNavigate();

    const gotoProjects = () => {
        navigate("/projects");
    };

    return(
        <div className = "landing-container">
            <h1 className="landing-title">
                Greenvale City Pollution Reduction Plan
            </h1>
            <button className = "landing-button" onClick= {gotoProjects}>
                View Projects
            </button>
        </div>
    );
}

export default LandingPage