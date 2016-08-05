
var incommingData = 
    [ "Things"
    , "and stuff"
    , "more stuff"
    , "right sure"
];

var MessageList = React.createClass(
    { render: function(){
        var toSpan = function(text){ return <span> {text} </span>; };
        return <div> {incommingData.map(toSpan)} </div>;
    }
    , _privateThing: function(){
    }
});

ReactDOM.render(<MessageList/>, document.getElementById('example'));
