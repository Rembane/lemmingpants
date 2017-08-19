'use strict';

import m from 'mithril';

var AttendeeList = {
    socket: null,
    init: function() {
        console.log(':D :D :D ');
        AttendeeList.socket = new WebSocket('ws://localhost:8000/');
        AttendeeList.socket.onopen = function(e) {
            console.log('open', e);
        }
        AttendeeList.socket.onmessage = function(e) {
            console.log(e.data);
        }
        AttendeeList.socket.onerror = function(e) {
            console.log('error', e);
        }
        AttendeeList.socket.onclose = function(e) {
            console.log('close', e);
        }
    }
}

export default AttendeeList;
