/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.iep.management.service;

import com.sun.jbi.engine.iep.management.api.IEPManagementService;
import javax.management.MBeanServerConnection;

/**
 *
 * @author rdwivedi
 */
public class IEPManagementServiceFactory {
    public static IEPManagementService createNewServiceInstance(MBeanServerConnection serverConnection) {
        return new IEPManagementServiceImpl(serverConnection);
    }

}
