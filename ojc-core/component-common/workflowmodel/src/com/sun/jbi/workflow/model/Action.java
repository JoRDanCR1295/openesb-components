/*
 * Action.java
 * 
 * Created on May 22, 2007, 1:10:33 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model;

import com.sun.jbi.workflow.model.xmlbeans.TActionType;

/**
 *
 * 
 */
public interface Action extends TaskElement, LocalNotificationContainer, ChangeVariableContainer {

    TActionType.Enum getType();
   
}
