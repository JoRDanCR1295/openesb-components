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
 * @(#)TabsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package woodstock.tabs;

import java.io.Serializable;

/**
 *
 * @author ylee
 */
public class TabsBean implements Serializable {
    
    private String message = "";
    private String url = "http://www.cnn.com";
    
    /** Creates a new instance of TabsBean */
    public TabsBean() {
    }
    
    public String tab1Clicked() {
        System.out.println("Tab1 clicked.");
        message = "tab1 clicked";
        url = "http://www.sun.com";
        return "";
    }
    
    public String tab2Clicked() {
        System.out.println("Tab2 clicked.");
        message = "tab2 clicked";
        url = "http://www.yahoo.com";
        return "";
    }
    
    public String tab3Clicked() {
        System.out.println("Tab3 clicked.");
        message = "tab3 clicked";
        url = "http://www.google.com";
        return "";
    }    
    
    public String getMessage() {
        return message;
    }
    
    public String getUrl() {
        return url;
    }
    
}
