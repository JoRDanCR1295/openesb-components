/*
 * @(#)FakeProjectConfiguration.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

import net.jcip.annotations.Immutable;
import org.openesb.components.rules4jbi.shared.config.SaveFailedException;
import org.openesb.components.rules4jbi.shared.config.Saveable;
import java.io.IOException;
import java.io.OutputStream;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Serializer;

/**
 * This class creates the content of project's fake <code>project.xml</code> configuration file.
 * It is a necessary hack to make the project work with the Composite Application project in NetBeans.
 * Right now, the composite application project is hardcoded to expect an ant-based project,
 * and is specifically looking for the directory <code>nbproject</code>, then for the file
 * <code>project.xml</code> underneath that directory, and is parsing out the value of
 * the <code>name</code> element, which needs to be that same as the project's name.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
@Immutable
public final class FakeProjectConfiguration implements Saveable {
    
    private final Document document;

    public FakeProjectConfiguration(String projectName) {
        document = new Document(createFakeProjectConfiguration(projectName));
    }
    
    static Element createFakeProjectConfiguration(String projectName) {
        Element project = new Element("project");
        
        Element name = new Element("name");
        name.appendChild(projectName);
        
        project.appendChild(name);
        
        return project;
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
