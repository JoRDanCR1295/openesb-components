/*
 * LocalNotificationContainer.java
 * 
 * Created on Jun 27, 2007, 2:47:28 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.workflow.model;

import java.util.List;

/**
 *
 * @author radval
 */
public interface LocalNotificationContainer {

    List<LocalNotification> getLocalNotifications();
}
