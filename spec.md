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

