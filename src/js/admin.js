'use strict';

var MeetingAdmin = {
    view: function() {
        return m('div', [
            m('section', [
                m('h2', [
                    m('button', 'PREVIOUS'),
                    '§x. Current item on the agenda.',
                    m('button', 'NEXT'),

                ]),
            ]),
            m('section', [
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
