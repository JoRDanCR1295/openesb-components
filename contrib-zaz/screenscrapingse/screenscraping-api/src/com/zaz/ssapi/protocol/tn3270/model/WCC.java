/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.zaz.ssapi.protocol.tn3270.model;

/**
 *
 * @author liyunhai
 */
public class WCC {

    private boolean reset = false;
    private boolean alarm = false;
    private boolean unlockKeyboard = false;
    private boolean resetModifiedDataTag = false;

    public void reset() {
        reset = false;
        alarm = false;
        unlockKeyboard = false;
        resetModifiedDataTag = false;
    }

    public boolean isAlarm() {
        return alarm;
    }

    public void setAlarm(boolean alarm) {
        this.alarm = alarm;
    }

    public boolean isReset() {
        return reset;
    }

    public void setReset(boolean reset) {
        this.reset = reset;
    }

    public boolean isResetModifiedDataTag() {
        return resetModifiedDataTag;
    }

    public void setResetModifiedDataTag(boolean resetModifiedDataTag) {
        this.resetModifiedDataTag = resetModifiedDataTag;
    }

    public boolean isUnlockKeyboard() {
        return unlockKeyboard;
    }

    public void setUnlockKeyboard(boolean unlockKeyboard) {
        this.unlockKeyboard = unlockKeyboard;
    }
}
