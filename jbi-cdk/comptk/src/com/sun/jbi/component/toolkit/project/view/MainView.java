/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)MainView.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.view;

import java.awt.BorderLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.text.MessageFormat;
import javax.swing.AbstractAction;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;
import javax.swing.event.ChangeEvent;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.model.Pom;
import com.sun.jbi.component.toolkit.project.model.Project;
import com.sun.jbi.component.toolkit.project.util.AsAdmin;
import com.sun.jbi.component.toolkit.project.util.FileCache;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.view.event.AppEvent;
import com.sun.jbi.component.toolkit.project.view.event.Support;
import com.sun.jbi.component.toolkit.project.view.event.AppEvent.Type;

/**
 * 
 * @author Kevan Simpson
 */
public class MainView extends JFrame implements App {
    public enum Tab { project, component, classpaths, poms, services }
    
    private Project mProject;
    private JTabbedPane mTabs;
    private ProjectTab mProjectTab;
    private ComponentTab mComponentTab;
    private ClasspathTab mClasspathTab;
    private PomTab mPomTab;
    private ServiceUnitTab mSrvcUnitTab;
    private MessagePanel mMessages;
    private SourcePane mSourcePane;
    private boolean[] mTabSaveActionEnabled;
    // cache of file content, key: absolute path
    private FileCache mFileCache;
    // AppEventListener support
    private Support mSupport;
    private volatile AbstractAction mSaveAction;
    private volatile AbstractAction mSaveAllAction;
    private volatile AbstractAction mExitAction;
    
	/** Default constructor. */
	public MainView(Project proj) {
		super();
		mProject = proj;
		updateTitle();
		mFileCache = new FileCache();
		mSupport = new Support();
        
        // These initializations must be done in the indicated order
        // due to inter-dependencies.
        initActions();
        initMenus();
        initLayout();
        updateActionStates();
        
        pack();
        setLocationRelativeTo(null);
        setResizable(false);
		showMessages(Status.good, "Welcome to the JBI Component Editor!");
	}
	
    /** @see com.sun.jbi.component.toolkit.project.view.App#getListenerSupport() */
    public Support getListenerSupport() {
        return mSupport;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.event.AppEventListener#handleEvent(com.sun.jbi.component.toolkit.project.view.event.AppEvent) */
    public void handleEvent(AppEvent evt) {
        if (evt != null) {
            DescriptionField fld = evt.getField();
//            System.out.println(fld.getXmlObject());
//            System.out.println(fld.getExpr().getExpression());
//            String oldValue = String.valueOf(pce.getOldValue());
//            System.out.println(oldValue +" - "+ evt);
            switch (evt.getType()) {
                case commit: {
                    // update XPathElement
                    XPathElement xpath = fld.getXPathElement();
                    if (xpath.setValue(fld.getExpr().getXPath(), fld.getText())) {
                        // DOM node updated ok
                        String xml = xpath.toXml(false);
                        getFileCache().saveFileChanges(xpath.getFile(), xml);
                        enableSave(true);
                        // fall through to load source
                    }
                    else {
                        break;
                    }
                }
                case display_source:    // show xml button
                case field_entered: {   // focus gained
                    mSourcePane.loadSource(fld);
                    break;
                }
//                case text_changed:
//                    mSourcePane.updateSource(fld);
//                    break;
                default: {
                    System.out.println("Ignoring "+ evt);
                    break;
                }
            }
        }
    }

    /** @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent) */
    public void stateChanged(ChangeEvent e) {
        if (e.getSource() instanceof JTabbedPane) {
            enableSave(false);
        }
    }

    /** @see com.sun.jbi.component.toolkit.project.view.App#getFileCache() */
    public FileCache getFileCache() {
        return mFileCache;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.App#getProject() */
    public Project getProject() {
        return mProject;
    }

    /** @see com.sun.jbi.component.toolkit.project.view.App#handleError(java.lang.Exception, java.lang.String, java.lang.Object[]) */
    public void handleError(Exception cause, String msg, Object... params) {
        // TODO i18n message & log stacktrace
        String err = MessageFormat.format(msg, params);
//        cause.printStackTrace();
        showMessages(Status.error, err);
        String[] msgs = Util.tokenize(err +"\n\n"+ Util.toString(cause), "\n");
        showMessages(Status.error, msgs);
    }

    /** @see com.sun.jbi.component.toolkit.project.view.App#showMessages(com.sun.jbi.component.toolkit.project.view.App.Status, java.lang.String[]) */
    public void showMessages(Status status, String... msgs) {
        switch (status) {
            case error:
            case warning: {
                MessagePanel.showMessageDialog(
                        "CDK Project "+ status.camel(), status, msgs);
                break;
            }
            default: {
                mMessages.addMessages(status, msgs);
                break;
            }
        }
    }

    /** @see java.awt.Component#setVisible(boolean) */
    @Override
    public void setVisible(boolean b) {
        super.setVisible(b);
        if (b) {
            // too lazy to figure out how to get these panels to display properly
            mSourcePane.fixSize();
            mMessages.fixSize();
            pack();
//            System.out.println(this.getSize());
        }
    }

    private void updateActionStates() {
        boolean enableSaveAll = false;
        for (boolean saveEnabled : mTabSaveActionEnabled) {
            if (saveEnabled) {
                enableSaveAll = true;
                break;
            }
        }
        mSaveAllAction.setEnabled(enableSaveAll);
        mSaveAction.setEnabled(mTabSaveActionEnabled[mTabs.getSelectedIndex()]);
    }
    
    private void enableSave(boolean set) {
        mTabSaveActionEnabled[mTabs.getSelectedIndex()] = set;
        updateActionStates();
    }
    
    private void exit() {
        // TODO if dirty, prompt user to save or cancel exit
        (new AsAdmin.AsAdminAction(
                "Exiting JBIC Console...", this, AsAdmin.stop_domain, -1))
                        .execute(getProject());
        System.exit(0);
    }
    
    private void initActions() {
        mSaveAction = new AbstractAction("Save") {
            public void actionPerformed(ActionEvent e) {
                save(Tab.values()[mTabs.getSelectedIndex()]);
            }
        };
        
        mSaveAllAction = new AbstractAction("Save All") {
            public void actionPerformed(ActionEvent e) {
                for (Tab tab : Tab.values()) {
                    save(tab);
                }
            }
        };
        
        mExitAction = new AbstractAction("Exit") {
            public void actionPerformed(ActionEvent e) {
                exit();
            }
        };
    }
    
	private void initLayout() {
		getContentPane().setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();

        // invariants for the layout
        // adding components top-down.
        c.insets = new Insets(5, 5, 5, 5);
        c.gridx = 0;
        c.gridy = GridBagConstraints.RELATIVE;
        c.weightx = 1.0;
        
		// CENTER - tabs
        JPanel pnlTabs = new JPanel(new BorderLayout());
		mProjectTab = new ProjectTab(this);
        mComponentTab = new ComponentTab(this);
        mClasspathTab = new ClasspathTab(this);
        mPomTab = new PomTab(this);
        mSrvcUnitTab = new ServiceUnitTab(this);
        mTabs = new JTabbedPane();
		mTabs.addTab("Project", mProjectTab);
		mTabs.addTab("Component", mComponentTab);
		mTabs.addTab("Classpaths", mClasspathTab);
		mTabs.addTab("POMs", mPomTab);
		mTabs.addTab("Service Units", mSrvcUnitTab);
		pnlTabs.add(mTabs, BorderLayout.CENTER);
		c.fill = GridBagConstraints.HORIZONTAL;
        c.anchor = GridBagConstraints.CENTER;
        c.weighty = 0.0;
		getContentPane().add(pnlTabs, c);
		mTabSaveActionEnabled = new boolean[mTabs.getTabCount()];
		mTabs.addChangeListener(this);
		
        // Source view
        c.fill = GridBagConstraints.BOTH;
        c.anchor = GridBagConstraints.CENTER;
        c.weighty = 1.0;
        mSourcePane = new SourcePane(this); // can't be null or listeners won't be added
        getContentPane().add(mSourcePane, c);

        // Status area
        c.fill = GridBagConstraints.BOTH;
        c.anchor = GridBagConstraints.CENTER;
        c.weighty = 0.0;
        mMessages = new MessagePanel(this, "Status", MESSAGE_DISPLAY_SIZE);
        getContentPane().add(mMessages, c);
        
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent e) {
				exit();
			}
		});
		getListenerSupport().fireAppEvent(
		        new AppEvent(Type.load_project, getProject()));
//        firePropertyChange(Event.load_project.toString(), null, getProject());
		
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	}
	
	private void initMenus() {
		JMenuBar menuBar;
		JMenu menu;
		JMenuItem menuItem;

		//Create the menu bar.
		menuBar = new JMenuBar();
        
        menu = new JMenu("File");
        menu.setMnemonic('F');
        
        menuItem = new JMenuItem(mSaveAction);
        menuItem.setMnemonic('S');
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_S, KeyEvent.CTRL_DOWN_MASK));
        menu.add(menuItem);
        
        menuItem = new JMenuItem(mSaveAllAction);
        menuItem.setMnemonic('A');
        menuItem.setAccelerator(KeyStroke.getKeyStroke(
                KeyEvent.VK_A, KeyEvent.CTRL_DOWN_MASK));
        menu.add(menuItem);
        
        menu.addSeparator();
        
        menuItem = new JMenuItem(mExitAction);
        menuItem.setMnemonic('X');
        menu.add(menuItem);
        
        menuBar.add(menu);
        
		setJMenuBar(menuBar);
	}

	private void save(Tab tab) {
        switch (tab) {
            case services:
            case project: {
                save(getProject());
                break;
            }
            case component: {
                save(getProject().getComponent());
                break;
            }
            case poms: {
                for (String name : getProject().getModuleNames()) {
                    Pom pom = getProject().getModule(name);
                    if (pom != null) {
                        save(pom);
                    }
                }
                break;
            }
        }
        enableSave(false);
	}
	
//    private void save(XmlObject<? extends Expr> xo) {
	private void save(XPathElement xo) {
        try {
            File file = xo.getFile();
            if (getFileCache().isModified(file)) {
                String content = getFileCache().getFileContent(file);
                if (!Util.isEmpty(content)) {
                    Util.writeFile(file, content);
                    showMessages(Status.info, 
                            "File saved successfully: "+ file.getAbsolutePath());
//                    firePropertyChange(Event.save_complete.toString(), xo, content);
                    getListenerSupport().fireAppEvent(
                            new AppEvent(Type.save_complete, xo));// TODO what's the source?!?
                }
            }
        }
        catch (Exception e) {
            showMessages(Status.error, 
                    "Failed to save to file: "+ xo.getFile().getAbsolutePath(),
                    "ERROR: "+ e.getMessage());
        }
    }

    private void updateTitle() {
        String title = "JBI Component Editor";
        if (getProject() == null) {
             title += " - No Project Loaded";
        }
        else {
            title += " - "+ getProject().getName();
        }
        
        setTitle(title);
    }
}
