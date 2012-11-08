/*
 * SEPluginProjectProperties.java
 */

package org.netbeans.modules.jbi.sample.engine.project;

import java.io.IOException;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.support.ant.AntBasedProjectType;
import org.netbeans.spi.project.support.ant.AntProjectHelper;

/**
 * Factory for simple service engine deployment plugin project projects.
 * @author chikkala
 */
public final class SEPluginProjectType implements AntBasedProjectType {

    /**
     */
    public static final String TYPE = "org.netbeans.modules.jbi.sample.engine.project.type"; // NOI18N
    /**
     */
    public static final String PROJECT_CONFIGURATION_NAMESPACE = "http://www.netbeans.org/ns/jbimodules/ProviderSE/1"; // NOI18N
    /**
     */
    public static final String PROJECT_CONFIGURATION_NAME = "data"; // NOI18N
    /**
     */
    public static final String PRIVATE_CONFIGURATION_NAME = "data"; // NOI18N
    /**
     */
    public static final String PRIVATE_CONFIGURATION_NAMESPACE = "http://www.netbeans.org/ns/jbimodules/ProviderSE/private/1"; // NOI18N

    /** Do nothing, just a service. */
    public SEPluginProjectType() {
    }

    public String getType() {
        return TYPE;
    }

    public Project createProject(AntProjectHelper helper) throws IOException {
        return new SEPluginProject(helper);
    }

    public String getPrimaryConfigurationDataElementName(boolean shared) {
        return shared ? PROJECT_CONFIGURATION_NAME : PRIVATE_CONFIGURATION_NAME;
    }

    public String getPrimaryConfigurationDataElementNamespace(boolean shared) {
        return shared ? PROJECT_CONFIGURATION_NAMESPACE : PRIVATE_CONFIGURATION_NAMESPACE;
    }
}
