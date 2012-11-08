/*
 * @(#)NewProjectWizardIterator.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
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

package org.openesb.components.rules4jbi.netbeans.project.wizard;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.project.compapp.FakeBuildScript;
import org.openesb.components.rules4jbi.netbeans.project.compapp.FakeProjectConfiguration;
import org.openesb.components.rules4jbi.netbeans.util.FileObjectSaver;
import org.openesb.components.rules4jbi.netbeans.util.wizard.AbstractWizardPanel;
import org.openesb.components.rules4jbi.shared.config.Configuration;
import java.io.File;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.logging.Logger;
import javax.swing.JComponent;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.spi.project.ui.support.ProjectChooser;
import org.openide.WizardDescriptor;
import org.openide.WizardDescriptor.Panel;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Mutex;
import org.openide.util.MutexException;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class NewProjectWizardIterator implements WizardDescriptor.InstantiatingIterator<WizardDescriptor> {

    private static final Logger logger = Logger.getLogger(NewProjectWizardIterator.class.getName());
    
    private int index = 0;
    
    private List<AbstractWizardPanel> panels;
    
    private WizardDescriptor settings;

    private NewProjectWizardIterator() {}
    
    public static NewProjectWizardIterator createIterator() {
        return new NewProjectWizardIterator();
    }
    
    private List<AbstractWizardPanel> createPanels() {
        List<AbstractWizardPanel> result = new ArrayList<AbstractWizardPanel>();
        
        result.add(new NameAndLocationWizardPanel());
        result.add(new RuleEngineWizardPanel());
        
        return result;
    }
    
    public void initialize(WizardDescriptor settings) {
        settings.putProperty(Constants.PROPERTY_PROJECT_NAME, Constants.DEFAULT_PROJECT_NAME);
        
        settings.putProperty(Constants.PROPERTY_PROJECT_LOCATION,
                ProjectChooser.getProjectsFolder().getAbsolutePath());

        settings.putProperty(Constants.PROPERTY_PROJECT_FOLDER,
                ProjectChooser.getProjectsFolder().getAbsolutePath()
                + File.separator
                + Constants.DEFAULT_PROJECT_NAME);
        
        settings.putProperty(Constants.PROPERTY_SET_AS_MAIN, Boolean.TRUE);
        settings.putProperty(Constants.PROPERTY_PROVIDER_URI, "");
        settings.putProperty(Constants.PROPERTY_PROVIDER_CLASS, "");
        
        this.settings = settings;
        
        index = 0;
        panels = createPanels();
        
        String[] panelNames = new String[panels.size()];
        
        for (int i = 0; i < panelNames.length; i++) {
            panelNames[i] = panels.get(i).getName();
        }

        logger.fine("Panel names are: " + Arrays.toString(panelNames));
        
        for (int i = 0; i < panels.size(); i++) {
            JComponent component = panels.get(i).getComponent();
            
            component.putClientProperty("WizardPanel_contentSelectedIndex", i);
            component.putClientProperty("WizardPanel_contentData", panelNames);
        }
    }
    
    public void uninitialize(WizardDescriptor settings) {
        logger.finest("Uninitializing new project wizard iterator");
        
        settings.putProperty(Constants.PROPERTY_PROJECT_NAME, null);
        settings.putProperty(Constants.PROPERTY_PROJECT_LOCATION, null);
        settings.putProperty(Constants.PROPERTY_PROJECT_FOLDER, null);
        
        /* Warning: uninitializing this property would set it back to default - Boolean.TRUE */
//        settings.putProperty(Constants.PROPERTY_SET_AS_MAIN, null);
        
        settings.putProperty(Constants.PROPERTY_PROVIDER_URI, null);
        settings.putProperty(Constants.PROPERTY_PROVIDER_CLASS, null);
        
        this.settings = null;
        panels = null;
    }
    
    public Set<?> instantiate() throws IOException {
        Map<String, Object> properties = settings.getProperties();
        logger.fine("Current properties: " + properties);
        
        File projectDirectory = new File((String) settings.getProperty(Constants.PROPERTY_PROJECT_FOLDER));
        projectDirectory = FileUtil.normalizeFile(projectDirectory);
        logger.fine("Creating project in: " + projectDirectory.getAbsolutePath());

        boolean created = projectDirectory.mkdir();
        logger.fine("Project directory " + (created ? "created successfully" : "already existed"));
        
        final Configuration configuration = new Configuration();
        
        configuration.setRuleServiceProvider(
                (String) settings.getProperty(Constants.PROPERTY_PROVIDER_URI));
        configuration.setRuleServiceProviderClass(
                (String) settings.getProperty(Constants.PROPERTY_PROVIDER_CLASS));
        
        final FileObject projectDirectoryFileObject = FileUtil.toFileObject(projectDirectory);
        
        final DirectoryManager directoryManager = new DirectoryManager(projectDirectoryFileObject);
        
        try {
            ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction<Void>() {

                public Void run() throws IOException {

                    directoryManager.createDirectoryStructure();

                    FileObject configFile = directoryManager.getConfigFile();
                    
                    FileObjectSaver.save(configuration, configFile);

                    FileObject fakeConfigFile = directoryManager.getFakeConfigFile();

                    FakeProjectConfiguration fakeProjectConfiguration =
                            new FakeProjectConfiguration(projectDirectoryFileObject.getName());
                    
                    FileObjectSaver.save(fakeProjectConfiguration, fakeConfigFile);

                    FileObject fakeBuildScriptFile = directoryManager.getFakeBuildScriptFile();

                    FakeBuildScript fakeBuildScript = new FakeBuildScript(projectDirectoryFileObject.getName());
                    
                    FileObjectSaver.save(fakeBuildScript, fakeBuildScriptFile);
                    
                    return null;
                }
            });

        } catch (MutexException e) {
            throw (IOException) e.getException();
        }
        
        File parentDirectory = projectDirectory.getParentFile();
        if (parentDirectory != null && parentDirectory.exists()) {
            ProjectChooser.setProjectsFolder(parentDirectory);
        }
        
        Set<FileObject> resultSet = new LinkedHashSet<FileObject>();
        resultSet.add(projectDirectoryFileObject);
        
        logger.fine("New project created successfully");
        return resultSet;
    }

    public String name() {
        return MessageFormat.format("{0} of {1}",
                new Object[] {new Integer(index + 1), new Integer(panels.size())});
    }
    
    public Panel<WizardDescriptor> current() {
        return panels.get(index);
    }

    public boolean hasNext() {
        return index < panels.size() - 1;
    }

    public boolean hasPrevious() {
        return index > 0;
    }

    public void nextPanel() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        
        index++;
    }

    public void previousPanel() {
        if (!hasPrevious()) {
            throw new NoSuchElementException();
        }
        
        index--;
    }

    public void addChangeListener(ChangeListener listener) {
        //do nothing
    }

    public void removeChangeListener(ChangeListener listener) {
        //do nothing
    }
}
