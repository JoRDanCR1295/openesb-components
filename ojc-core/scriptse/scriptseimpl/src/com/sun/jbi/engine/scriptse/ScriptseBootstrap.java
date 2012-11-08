/* *************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.engine.scriptse;

import com.sun.jbi.crl.lifecycle.BootstrapFilterChain;
import com.sun.jbi.crl.lifecycle.ConfigPersistenceFilter;
import com.sun.jbi.crl.lifecycle.DefaultBootstrapFilter;

import javax.management.NotCompliantMBeanException;
import javax.management.StandardMBean;


/**
 * Bootstrap implementation for Script Service Engine.
 *
 * @author Prashanth B.R
 */
public class ScriptseBootstrap extends BootstrapFilterChain { //implements Bootstrap {

    /**
     * Creates a new ScriptseBootstrap object.
     */
    public ScriptseBootstrap() {
        StandardMBean mbean = null;

        try {
            mbean = new StandardMBean(
                    new ScriptseInstallerConfigurationMBean(), ScriptseConfigurationMBean.class
                );
        } catch (NotCompliantMBeanException ncme) {
            // log error
        }

        addFilter(new DefaultBootstrapFilter(this, mbean));
        addFilter(new ConfigPersistenceFilter(this));
    }
}
