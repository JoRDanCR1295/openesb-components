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
 * @(#)Support.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view.event;

import java.util.ArrayList;
import java.util.List;
import com.sun.jbi.component.toolkit.project.view.MessagePanel;
import com.sun.jbi.component.toolkit.project.view.App.Status;

/**
 * 
 * @author Kevan Simpson
 */
public class Support {
    private List<AppEventListener> mListeners;
    
    public Support() {
        mListeners = new ArrayList<AppEventListener>();
    }
    
    public void addEventListener(AppEventListener ael) {
        if (ael != null) {
            mListeners.add(ael);
        }
    }

    public void fireAppEvent(AppEvent evt) {
        if (evt != null) {
            for (AppEventListener ael : mListeners) {
                try {
                    ael.handleEvent(evt);
                }
                catch (Exception e) {
                    MessagePanel.showMessageDialog(
                            "AppEvent Error!", Status.error, 
                            ael.getClass().getSimpleName() +
                            " failed to handle event: "+ String.valueOf(evt));
                }
            }
        }
    }
    
    public void removeEventListener(AppEventListener ael) {
        if (ael != null) {
            mListeners.remove(ael);
        }
    }
}
