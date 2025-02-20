import React from 'react';
import { Link } from 'react-router-dom';
import Swal from 'sweetalert2';

class UserProfile extends React.Component {
    constructor() {
        super();
        localStorage.setItem("view", 1);
        if(localStorage.getItem("users")===null){
            let tempList = [];
            localStorage.setItem("users", JSON.stringify(tempList));
        }
        this.state = {
            name : ""
        }
    }
    handleChange = event => {
        this.setState({ name: event.target.value });
    };
    updateUsers(){

        let tempUsers = JSON.parse(localStorage.getItem('users'));
        let alreadyInUsers = false;
        for (let i = 0; i<tempUsers.length;i++){
            if(tempUsers[i].Name === this.state.name) {
                alreadyInUsers = true;
                break;
            }
        }
        if (!alreadyInUsers){
            let emptyDict = {
                Name: this.state.name,
                Preferences: ["vegan", "vegetarian", "gluten-free"],
                Allergies: ["nuts", "lactose", "seafood"],
                Log: [],
                Liked: [],
                Disliked: [],
                FirstTime: true,
                FirstTime1: true,
                FirstTime2: true,
                booleansPreferences: [false, false, false],
                booleanAllergies: [false, false, false]
            };
            tempUsers.push(emptyDict);
            localStorage.setItem('users', JSON.stringify(tempUsers));
        }

        localStorage.setItem('currentUser', this.state.name);
    }

    handleTest(e) {

        if (e.ctrlKey && e.charCode == 13 ) {
            localStorage.setItem("view", ((JSON.parse( localStorage.getItem("view")))+1)%2);
        }
    }
    render() {

        return (

            <div className="App">
                <header className="App-header">
                    <div className="DarkBackground">
                        <div>
                            <h4 className="fadeInLeft0">Hello there! Thank you for wanting to participate in our test.</h4>
                            <h4 className="fadeInLeft1"> Before we start, can we please get your name?</h4>
                            <input
                                type="text"
                                name="username"
                                className={"textBox fadeInLeft2"}
                                placeholder={"Your name"}
                                value={this.state.username}
                                onChange={this.handleChange}
                                onKeyPress={this.handleTest}
                            />

                        </div>
                        <Link to="/home"><button className="NextButton White fadeInLeft3"
                             onClick={
                                ()=>{this.updateUsers()}
                             }>
                        <b>NEXT</b></button></Link>
                    </div>
                </header>
            </div>

        );
    }
}

export default UserProfile;
