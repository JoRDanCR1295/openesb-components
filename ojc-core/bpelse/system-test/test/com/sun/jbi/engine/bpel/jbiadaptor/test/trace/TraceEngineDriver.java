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
 * @(#)TraceEngineDriver.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.jbiadaptor.test.trace;

import java.io.File;
import java.io.FileInputStream;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineDriver;

/**
 *
 *
 * @author Sun Microsystems
 */
public class TraceEngineDriver extends EngineDriver {
	
	/* */
	private FileHandler mFileHandler;
	
	/* */
	private Logger mLogger;

	/**
	 * 
	 * @throws Exception
	 */
	public TraceEngineDriver() throws Exception {
		super();
	}

	@Override
	protected void runTest(String[] args) throws Exception {
		
		mLogger = Logger.getLogger(BPELTraceManager.BPEL_TRACE_CATEGORY);
		String outFilePath = mProps.getProperty(ACT_OUT_FILE_PATH);
        mFileHandler = new FileHandler(outFilePath);
        TestFormatter testFormatter = new TestFormatter();
        mFileHandler.setFormatter(testFormatter);
        mLogger.addHandler(mFileHandler);
        
		try {
			super.runTest(args);
		} finally {
			// remove the fileHandler associated with the logger.
			mLogger.removeHandler(mFileHandler);
			mFileHandler.flush();
			mFileHandler.close();
		}
	}

	@Override
	protected void postRunVerification(String[] args) throws Exception {
    	File actOutFile = new File(mProps.getProperty(ACT_OUT_FILE_PATH));
    	File expOutFile = new File(mProps.getProperty(EXP_OUT_FILE_PATH));
    	compareTextFiles(expOutFile, actOutFile);
	}
		
	/*
	 * 
	 */
	private static class TestFormatter extends Formatter {

		/* */
		private static final String LINE_SEPARATOR = System.getProperty("line.separator", "\n");
		
		@Override
		public String format(LogRecord record) {
			String log = record.getLevel().toString() + " : " + record.getMessage() + LINE_SEPARATOR;
			return log;
		}
		
	}
}
