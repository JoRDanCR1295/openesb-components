/*
 * @(#)Rules4JBIAntArtifactProvider.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.project;

import org.openesb.components.rules4jbi.netbeans.project.compapp.FakeBuildScript;
import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.logging.Logger;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ant.AntArtifact;
import org.netbeans.spi.project.ant.AntArtifactProvider;
import org.openesb.components.rules4jbi.shared.GlobalConstants;
import org.openide.filesystems.FileUtil;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class Rules4JBIAntArtifactProvider implements AntArtifactProvider {
    
    private static final Logger logger = Logger.getLogger(Rules4JBIAntArtifactProvider.class.getName());
    
    private final Rules4JBIProject project;

    public Rules4JBIAntArtifactProvider(Rules4JBIProject project) {
        this.project = project;
    }

    public AntArtifact[] getBuildArtifacts() {
        logger.fine("Creating build artifacts");
        
        return new AntArtifact[] {
            new FakeArtifact("CAPS.asa:" + GlobalConstants.SERVICE_ENGINE_NAME,
                    FakeBuildScript.DIST_TARGET_NAME, project),
            new FakeArtifact(JavaProjectConstants.ARTIFACT_TYPE_JAR, "ID2", project)
        };
    }
    
    private static class FakeArtifact extends AntArtifact {

        private final String type;
        
        private final String id;
        
        private final Project project;

        public FakeArtifact(String type, String id, Project project) {
            this.type = type;
            this.id = id;
            this.project = project;
        }

        @Override
        public URI[] getArtifactLocations() {
//            File file = new File(FileUtil.toFile(project.getProjectDirectory()), "build/SEDeployment.jar");
            
            try {
                URI result = new URI(null, null, "build/SEDeployment.jar", null);

                logger.fine(type + ": returned artifact location: " + result);

                return new URI[] {result};
                
            } catch (URISyntaxException e) {
                logger.severe(type + ": failed to provide artifact location: " + e.getMessage());
                
                throw new AssertionError("The URI provided is valid");
            }
        }
        
        @Override
        public String getID() {
            return id;
        }

        @Override
        public Project getProject() {
            return project;
        }
        
        @Override
        public String getType() {
            return type;
        }

        @Override
        public File getScriptLocation() {
            File scriptLocation = new File(FileUtil.toFile(project.getProjectDirectory()), "build.xml");
            
            logger.fine(type + ": retrieving script location: " + scriptLocation.getAbsolutePath());
            
            return scriptLocation;
        }

        @Override
        public String getTargetName() {
            logger.fine(type + ": returning dist target name");
            
            return FakeBuildScript.DIST_TARGET_NAME;
        }

        @Override
        public String getCleanTargetName() {
            logger.fine(type + ": returning clean target name");
            
            return FakeBuildScript.CLEAN_TARGET_NAME;
        }
    }
}
