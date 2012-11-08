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
 * @(#)App.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.Color;
import javax.swing.event.ChangeListener;
import com.sun.jbi.component.toolkit.project.model.Project;
import com.sun.jbi.component.toolkit.project.util.FileCache;
import com.sun.jbi.component.toolkit.project.view.event.AppEventListener;
import com.sun.jbi.component.toolkit.project.view.event.Support;



/**
 * The application controller. 
 * @author Kevan Simpson
 */
public interface App extends AppEventListener, ChangeListener {

    public static final int MESSAGE_DISPLAY_SIZE = 4;
    
    public enum Status { 
        good(Color.black), 
        info(Color.green.darker().darker()), 
        warning(Color.orange.darker()), 
        error(Color.red.darker());
        
        private Color mColor;
        private Status(Color c) {
            mColor = c;
        }
        
        public Color getColor() {
            return mColor;
        }
        
        public String camel() {
            String str = this.toString();
            return Character.toUpperCase(str.charAt(0)) + str.substring(1);
        }
    }

    public FileCache getFileCache();
    public Support getListenerSupport();
    public Project getProject();
    public void handleError(Exception cause, String msg, Object... params); 
    public void showMessages(Status status, String... msgs);
}
