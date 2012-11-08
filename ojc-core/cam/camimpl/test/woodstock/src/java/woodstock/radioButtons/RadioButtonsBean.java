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
 * @(#)RadioButtonsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package woodstock.radioButtons;

import java.io.Serializable;

/**
 *
 * @author ylee
 */
public class RadioButtonsBean implements Serializable {
    boolean button1;
    boolean button2;
    boolean button3;
    
    /** Creates a new instance of RadioButtonsBean */
    public RadioButtonsBean() {
    }
    
    public void setButton1Selected(boolean b) {
        button1 = b;
        System.out.println("set button1: "+button1);
    }
    
    public boolean getButton1Selected() {
        System.out.println("get button1: "+button1);
        return button1;
    }

    
    public void setButton2Selected(boolean b) {
        button2 = b;
        System.out.println("set button2: "+button2);
    }
    
    public boolean getButton2Selected() {
        System.out.println("get button2: "+button2);
        return button2;
    }
    
    public void setButton3Selected(boolean b) {
        button3 = b;
        System.out.println("button3: "+button3);
    }
    
    public boolean getButton3Selected() {
        System.out.println("button3: "+button3);        
        return button3;
    }
    
    public String actionHandler() {
        System.out.println("actionHandler called...");
        System.out.println("button1: "+button1+" button2: "+button2+" button3: "+button3);
        return "";
    }
}
