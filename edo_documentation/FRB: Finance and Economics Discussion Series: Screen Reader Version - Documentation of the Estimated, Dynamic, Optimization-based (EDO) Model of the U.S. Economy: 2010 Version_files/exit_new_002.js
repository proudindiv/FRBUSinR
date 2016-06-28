/*
function exitalert(url)  {
  adjWidth = 500;
  adjHeight = 350;
  var thisURL = encodeURIComponent(url);
  theWindow=window.open('/exit_win_new.cfm?url=' + thisURL + '','windowName','width=' + adjWidth + ',height=' +      adjHeight + ',top=100,left=100,toolbar=no,location=no,directories=no,status=no,menubar=no,scrollbars=no')
}

*/

//-------------------------Exit Popup

//Include code from an external js
document.write('<scr' + 'ipt type="text/javascript" src="/resources/exit_new.js" ></scr' + 'ipt>');
//-------------------------Exit Popup
function exitalert(url) {
    var whichExitText = "ext";

    var TheExit = window.open("", "_blank", "toolbar=no, location=no, directories=no,status=no,menubar=no,scrollbars=no, resizable=no,copyhistory=no,width=850,height=500,left=200,top=100,screen-X=0,screen-Y=0")
    TheExit.document.write(Dialog(url, whichExitText));
    TheExit.document.close();
}


/*
function testlinkalert2()  {
  adjWidth = 500;
  adjHeight = 350;
  var thisURL = window.location.href;
  theWindow=window.open('exitw_test.cfm?url=' + thisURL + '','windowName','width=' + adjWidth + ',height=' +      adjHeight + ',top=100,left=100,toolbar=no,location=no,directories=no,status=no,menubar=no,scrollbars=no')
}
*/

function open_new_win(url)  {
    newWindow = window.open(url,"SubForm","toolbar=no,width=500,height=350,top=100,left=100,directories=no,status=no,scrollbars=yes,resizable=no,menubar=no")
} 