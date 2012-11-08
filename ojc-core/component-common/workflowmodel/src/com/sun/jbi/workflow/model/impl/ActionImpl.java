/*
 * ActionImpl.java
 * 
 * Created on May 22, 2007, 1:46:06 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.apache.xmlbeans.impl.values.XmlValueOutOfRangeException;

import com.sun.jbi.workflow.model.Action;
import com.sun.jbi.workflow.model.ChangeVariables;
import com.sun.jbi.workflow.model.LocalNotification;
import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.xmlbeans.TAction;
import com.sun.jbi.workflow.model.xmlbeans.TActionType;
import com.sun.jbi.workflow.model.xmlbeans.TChangeVariables;
import com.sun.jbi.workflow.model.xmlbeans.TLocalNotification;


/**
 * 
 * 
 */
public class ActionImpl extends ModelElementImpl implements Action {

    private TAction mActionType;

    private List<LocalNotification> mLocalNotifications = new ArrayList<LocalNotification>();
    
    private List<ChangeVariables> mChangeVariables = new ArrayList<ChangeVariables> ();

    public ActionImpl(TAction action, ModelElement parent) {
        super(action, parent);
        this.mActionType = action;
        init();
    }

    private void init() {
        List<TLocalNotification> lnList = this.mActionType
                .getLocalNotificationList();
       List<TChangeVariables> varList = this.mActionType.getChangeVariablesList();
        if (lnList != null && lnList.size() > 0) {
            Iterator<TLocalNotification> it = lnList.iterator();
            while (it.hasNext()) {
                TLocalNotification ln = it.next();
                LocalNotification localNotification = new LocalNotificationImpl(
                        ln, this);
                this.mLocalNotifications.add(localNotification);
            }
        }
        
        if (varList != null && varList.size() > 0) {
            for (TChangeVariables var : varList) {
                ChangeVariables chvar = new ChangeVariablesImpl (var, this);
                this.mChangeVariables.add(chvar);
            }
        }
            
    }

    public TActionType.Enum getType() {
        return this.mActionType.getType();
    }

    public List<LocalNotification> getLocalNotifications() {

        return this.mLocalNotifications;
    }

    public  List<ChangeVariables> getChangeVariables () {
        // TODO Auto-generated method stub
        return mChangeVariables;
    }

}
