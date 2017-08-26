'use strict';

import m from 'mithril';
import Agenda from './Agenda';
import AttendeeList from './AttendeeList';

var AgendaComponent = {
    oninit: Agenda.init,
    view: function() {
        if(Agenda.agenda !== null && Agenda.agenda.length > 0) {
            console.log(Agenda.agenda);
            return m('h2', [
                m('button', { onclick: function() { Agenda.decCurrent(); } }, '|<'),
                m.trust('&sect;'),
                (Agenda.current + 1) + '. ',
                Agenda.getCurrent().title,
                m('button', { onclick: function() { Agenda.incCurrent(); } }, '>|')
            ]);
        } else {
            return m('p[class=error]', 'There is no agenda... please load one or reload the page.');
        }
    }
}

var AttendeeListComponent = {
    visible: false,
    oninit: AttendeeList.init,
    view: function() {
        return m(':D');
    }
}

var MeetingAdmin = {
    view: function() {
        return m('div', [
            m('section', [
                m(AgendaComponent)
            ]),
            m('section', [
                m(AttendeeListComponent),
                m('p', [
                    m('a[href=""]', { onclick: function() {  } }, 'Show list of attendees.')
                ]),
                m('p', [
                    m('label', [
                        'Number',
                        m('br'),
                        m('input', { type: 'text' })
                    ])
                ]),
                m('p', [
                    m('input', { type: 'submit', value: 'Add to speaker list!' })
                ])
            ]),
            m('section', [
                m('h2', 'First'),
                m('ol', [
                    m('li', [
                        m('button', 'DEL'),
                        'Bob Bobson'
                    ]),
                    m('li', [
                        m('button', 'DEL'),
                        'Eric Ericson'
                    ]),
                    m('li', [
                        m('button', 'DEL'),
                        'Mc Hammer'
                    ]),
                ])
            ]),
            m('section', [
                m('h2', 'Second'),
                m('ol', [
                    m('li', [
                        m('button', 'DEL'),
                        'Woody Woodpecker'
                    ]),
                    m('li', [
                        m('button', 'DEL'),
                        'Doland Dcuk'
                    ]),
                ])
            ]),
            m('section', [
                m('button', 'Push speaker list'),
                m('button', 'Pop speaker list'),
            ])
        ]);
    }
};
m.mount(document.body, MeetingAdmin);
