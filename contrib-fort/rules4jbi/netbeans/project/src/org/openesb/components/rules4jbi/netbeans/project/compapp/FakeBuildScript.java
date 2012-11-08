/*
 * @(#)FakeBuildScript.java        $Revision: 1.2 $ $Date: 2008/11/24 12:47:19 $
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

package org.openesb.components.rules4jbi.netbeans.project.compapp;

import org.openesb.components.rules4jbi.shared.config.SaveFailedException;
import org.openesb.components.rules4jbi.shared.config.Saveable;
import java.io.IOException;
import java.io.OutputStream;
import nu.xom.Attribute;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Serializer;

import net.jcip.annotations.Immutable;

/**
 * This class creates the content of a project's fake ant build script. It is a necessary
 * hack to make the project work with the Composite Application project in NetBeans.
 * Right now, the composite application project is hardcoded to expect an ant-based project,
 * and is looking for an <code>AntArtifactProvider</code> in project's lookup, to find
 * the generated artifact. Which is also hardcoded and has to be under the <code>build</code>
 * directory and has to be called <code>SEDeployment.jar</code> for a service engine artifact.
 * The composite application then calls this build script's targets to clean and build the project,
 * when the whole composite application gets build.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/11/24 12:47:19 $
 * 
 * @see org.netbeans.api.project.ant.AntArtifact
 * @see org.netbeans.spi.project.ant.AntArtifactProvider
 * @since 0.1
 */
@Immutable
public final class FakeBuildScript implements Saveable {
    
    public static final String CLEAN_TARGET_NAME = "clean";
    
    public static final String DIST_TARGET_NAME = "dist_se";
    
    static final String CLEAN_TARGET_MESSAGE = "Warning: this is not an Ant-based project;"
            + " please clean the project manually";

    static final String DIST_TARGET_MESSAGE = "Warning: this is not an Ant-based project;"
            + " please build the project manually";
    
    private final Document document;
    
    public FakeBuildScript(String projectName) {
        document = new Document(createFakeBuildScript(projectName));
    }

    static Element createFakeBuildScript(String projectName) {
        Element project = new Element("project");
        
        Attribute name = new Attribute("name", projectName);
        project.addAttribute(name);

        Attribute defaultAttribute = new Attribute("default", DIST_TARGET_NAME);
        project.addAttribute(defaultAttribute);

        Attribute basedir = new Attribute("basedir", ".");
        project.addAttribute(basedir);
        
        Element cleanTarget = createTarget(CLEAN_TARGET_NAME, CLEAN_TARGET_MESSAGE);
        
        Element distTarget = createTarget(DIST_TARGET_NAME, DIST_TARGET_MESSAGE);

        project.appendChild(cleanTarget);
        
        project.appendChild(distTarget);

        return project;
    }
    
    static Element createTarget(String name, String message) {
        Element target = new Element("target");

        Attribute nameAttribute = new Attribute("name", name);
        target.addAttribute(nameAttribute);

        Element echo = new Element("echo");

        Attribute messageAttribute = new Attribute("message", message);
        echo.addAttribute(messageAttribute);
        
        target.appendChild(echo);
        
        return target;
    }
    
    public void save(OutputStream outputStream) throws SaveFailedException {
        try {
            Serializer serializer = new Serializer(outputStream, "UTF-8");
            serializer.setIndent(4);
            serializer.write(document);

        } catch (IOException e) {
            throw new SaveFailedException(e);
        }
    }
}
