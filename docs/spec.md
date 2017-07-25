---
title: Lemming pants – specification
author:
- Andreas Ekeroot
abstract: |
    This document describes how the speaker list system for Datateknologsektionen should work.
...

# Registration

Members of Datateknologsektionen registers themselves in the system on their cell phones or on a terminal placed in the meeting hall. The system responds with either "CID not found" or "This is your speaker id: \[Integer\]".

## Sketch

<div class="example">
    <p>
        <label>CID
            <br><input type="text" />
        </label>
    </p>

    <p>
        <input type="submit" value="Register!">
    </p>
</div>

The meeting participant then grabs a paper plate or paper and writes the number on that. That token is used during the meeting to identify the participant in the system.

# The meeting view

This view is shown to all meeting participants during the meeting, preferably on a projector or huge tv-monitor. But it won't hurt if it's usable on cellphones.

<div class="example">
    <h2>§x. Current item on the agenda.</h2>
    <table>
        <tr>
            <td style="vertical-align:top;">
                <ol>
                    <li>Bob Bobson</li>
                    <li>Eric Ericson</li>
                    <li>Mc Hammer</li>
                    <li>...</li>
                </ol>
            </td>

            <td style="vertical-align:top;">
                <ol>
                    <li>Woody Woodpecker</li>
                    <li>Doland Dcuk</li>
                </ol>
            </td>
        </tr>
    </table>
</div>

# Manage meeting

We need to be able to at any time in the meeting interrupt the speaker list and the meeting with an `ordningsfråga` (meta meeting matter), and in the worst case create a new speaker list for this matter. We will model this as a stack, so when an `ordningsfråga` is raised, push a new speaker list to the stack, work with that, and when it is done, pop the stack and go back to the previous speaker list.

<div class="example">
    <table>
        <tr>
            <td style="vertical-align:top;" colspan="2">
                <h2>
                <button>PREVIOUS</button>
                §x. Current item on the agenda.
                <button>NEXT</button>
                </h2>
            </td>
        </tr>
        <tr>
            <td style="vertical-align:top;">
                <p>
                    <label>CID
                        <br><input type="text" />
                    </label>
                </p>

                <p>
                    <input type="submit" value="Add to speaker list!">
                </p>
            </td>
        </tr>
        <tr>
            <td style="vertical-align:top;">
                <h2>First</h2>
                <ol>
                    <li><button>DEL</button> Bob Bobson</li>
                    <li><button>DEL</button> Eric Ericson</li>
                    <li><button>DEL</button> Mc Hammer</li>
                    <li>...</li>
                </ol>
            </td>
            <td style="vertical-align:top;">
                <h2>Second</h2>
                <ol>
                    <li><button>DEL</button> Woody Woodpecker</li>
                    <li><button>DEL</button> Doland Dcuk</li>
                </ol>
            </td>
        </tr>
    </table>

    <button>Push speaker list</button> <button>Pop speaker list</button>
</div>
