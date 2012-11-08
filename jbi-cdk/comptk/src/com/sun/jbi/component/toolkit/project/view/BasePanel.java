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
 * @(#)BasePanel.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.util.ArrayList;
import java.util.List;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.view.event.AppEvent;
import com.sun.jbi.component.toolkit.project.view.event.AppEventListener;
import com.sun.jbi.component.toolkit.project.view.event.Support;
import com.sun.jbi.component.toolkit.project.view.event.AppEvent.Type;

/**
 * 
 * @author Kevan Simpson
 */
public abstract class BasePanel extends JPanel implements AppEventListener {
    public static final String EMPTY = "<html>&nbsp;</html>";
    
    public enum ActionText { Create, Modify, Delete }
    
    private App mController;
    private List<DescriptionField> mFields;
    private Support mSupport;
    
    protected BasePanel(App app, Object... params) {
        mController = app;
        mFields = new ArrayList<DescriptionField>();
        mSupport = new Support();
        if (app != null) {  // see MessagePanel.showMessageDialog
            mSupport.addEventListener(app);
            app.getListenerSupport().addEventListener(this);
        }
        init(params);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.event.AppEventListener#handleEvent(com.sun.jbi.component.toolkit.project.view.event.AppEvent) */
    public void handleEvent(AppEvent evt) {
        if (evt.getType() == Type.load_project) {
            updateValues();
        }
    }

    protected void addField(DescriptionField fld) {
        if (fld != null) {
            mFields.add(fld);
        }
    }
    
    protected boolean confirmDelete(String type, String name) {
        int opt = JOptionPane.showConfirmDialog(null, 
                "Are you sure you want to delete "+ type +": "+ name, 
                "Confirm Delete?", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
        return (opt == JOptionPane.YES_OPTION);
    }
    
    protected Support getListenerSupport() {
        return mSupport;
    }
    
    protected boolean updateFields(XPathElement xpath) {
        if (xpath != null) {
            for (DescriptionField fld : mFields) {
                fld.setXPathElement(xpath);
            }
            
            return true;
        }
        
        return false;
    }
    
    protected boolean updateValues() {
//        System.out.println("updateValues: "+ this.getClass().getName());
        return getApp().getProject() != null;
    }
    
    protected abstract void init(Object... params);

    /** 
     * Returns the controller.
     * @return the controller. 
     */
    protected App getApp() {
        return mController;
    }

    protected void fixSize() {
    }
}
