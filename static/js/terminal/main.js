'use strict';

import m from 'mithril';

var number = '';
var cid    = '';
var postCID = function() {
    m.request({
        method: 'POST',
        url: '/attendee/create',
        data: cid,
        headers: {
            'Accept': 'application/json',
            'Content-type': 'application/json'
        }
    }).then(function(result) {
        number = result;
    });
}

var NewAttendee = {
    view: function() {
        return m('section', [
            m('p', 'TODO: Add error handling!'),
            m('form', {
                onsubmit: function(e) {
                    e.preventDefault();
                    postCID();
                }
            }, [
                m('h1', 'Attend meeting!'),
                    m('p', [
                        m('label', [
                            'Enter Your CID:',
                            m('br'),
                            m('input[type=text]', { oninput: m.withAttr('value', function(value) { cid = value; }), value: cid })
                        ])
                    ]),
                    m('button[type=Submit]', 'Submit!')
            ]),
            m('h1', number)
        ]);
    }
}
m.mount(document.body, NewAttendee);
