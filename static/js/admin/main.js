'use strict';

import m from 'mithril';
import Agenda from './Agenda';
import AttendeeList from './AttendeeList';

var AgendaComponent = {
    oninit: Agenda.init,
    view: function() {
        try {
            return m('section', [
                m('h2', [
                    m('button', { onclick: function() { Agenda.decCurrent(); } }, '|<'),
                    m.trust('&sect;'),
                    (Agenda.current + 1) + '. ',
                    Agenda.getCurrent().title,
                    m('button', { onclick: function() { Agenda.incCurrent(); } }, '>|')
                ])
            ]);
        } catch(e) {
            if(e.name === 'NotLoadedYetException') {
                return '';
            } else {
                throw e;
            }
        }
    }
}

var AttendeeListComponent = {
    visible: false,
    //oninit: AttendeeList.init,
    view: function() {
        return m('section', [
            m('p', [
                m('a[href=""]', { onclick: function() {  } }, 'Show list of attendees.')
            ])
        ]);
    }
}

var SpeakerQueueStackComponent = {
    view: function() {
        try {
            return m('section', [
                m('h1', 'Speakers'),
                m('ol', Agenda.getCurrent()['speakerQueueStack'].list().map(function(a) {
                    return m('li', [
                        m('button', 'DEL'),
                        a
                    ]);
                })),
                m('p', ['Speaker stack height: ', Agenda.getCurrent()['speakerQueueStack'].stackHeight()]),
                m('button', { onclick: function() { Agenda.getCurrent()['speakerQueueStack'].push(); } }, 'Push speaker queue'),
                m('button', 'Pop speaker queue'),
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
            ]);
        } catch(e) {
            if(e.name === 'NotLoadedYetException') {
                return '';
            } else {
                throw e;
            }
        }
    }
}

var MeetingAdmin = {
    view: function() {
        return [
            m(AgendaComponent),
            m(AttendeeListComponent),
            m(SpeakerQueueStackComponent)
        ];
    }
};
m.mount(document.body, MeetingAdmin);
