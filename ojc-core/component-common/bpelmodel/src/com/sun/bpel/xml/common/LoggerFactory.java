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
 * @(#)LoggerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common;

//import org.apache.log.Logger;
//import org.apache.log.Priority;
//import org.apache.log.Hierarchy;
//
//import org.apache.log.format.ExtendedPatternFormatter;
//import org.apache.log.output.io.FileTarget;
//import org.apache.log.output.io.StreamTarget;
//
//import java.io.File;
//import java.io.IOException;
//import java.io.OutputStream;

/**
 * LoggerFactory a utilitye class for getting a logging object using the apache
 * LogKit Usage example: <CODE>Logger logger = LoggerFactory.getLogger();
 * logger.log(Priority.DEBUG, "This is a debug message" );
 * logger.log(Priority.ERROR, "This is an error message", ex ); </CODE>
 *
 * @author Sun Microsystems
 * @version 
 *
 * @since 5.0
 */
public final class LoggerFactory {
//    /** DOCUMENT_ME */
//    private static final String DEFAULT_FORMAT_PATTERN =
//        "%5.5{priority} %{method}"
//        + "[%8.8{category}] : %{message}\\n%{throwable}";
//
//        /**
//         * Creates a new Logger object, using default pattern and prioroty of
//         * DEBUG
//         *
//         * @return Logger
//         */
//        public static Logger getLogger() {
//            return getLogger(null, null, null);
//        }
//
//        /**
//         * Creates a new Logger object, using default pattern and system out
//         *
//         * @param priority the Priority level
//         *
//         * @return Logger
//         */
//        public static Logger getLogger(Priority priority) {
//            return getLogger(priority, null, null);
//        }
//
//        /**
//         * Creates a new Logger object, using system out
//         *
//         * @param priority the Priority level
//         * @param pattern the format pattern
//         *
//         * @return Logger
//         */
//        public static Logger getLogger(Priority priority, String pattern) {
//            return getLogger(priority, pattern, null);
//        }
//
//        /**
//         * Creates a new Logger object
//         *
//         * @param priority the Priority level
//         * @param pattern the format pattern
//         * @param output the output stream for the message
//         *
//         * @return Logger
//         */
//        public static Logger getLogger(Priority priority, String pattern,
//            OutputStream output) {
//            ExtendedPatternFormatter formatter = null;
//            StreamTarget target = null;
//
//            if (pattern == null) {
//                formatter =
//                    new ExtendedPatternFormatter(DEFAULT_FORMAT_PATTERN);
//            } else {
//                formatter = new ExtendedPatternFormatter(pattern);
//            }
//
//            if (output == null) {
//                target = new StreamTarget(System.out, formatter);
//            } else {
//                target = new StreamTarget(output, formatter);
//            }
//
//            Hierarchy.getDefaultHierarchy().setDefaultLogTarget(target);
//
//            Logger logger =
//                Hierarchy.getDefaultHierarchy().getLoggerFor("Default");
//
//            if (priority == null) {
//                logger.setPriority(priority);
//            } else {
//                logger.setPriority(Priority.DEBUG);
//            }
//
//            return logger;
//        }
//
//        /**
//         * Creates a new File Logger object
//         *
//         * @param priority the Priority level
//         * @param pattern the format pattern
//         * @param fileName the file name and path for the output messages
//         *
//         * @return Logger
//         *
//         * @throws IOException DOCUMENT_ME
//         */
//        public static Logger getFileLogger(Priority priority, String pattern,
//            String fileName) throws IOException {
//            ExtendedPatternFormatter formatter = null;
//            FileTarget target = null;
//
//            if (pattern == null) {
//                formatter =
//                    new ExtendedPatternFormatter(DEFAULT_FORMAT_PATTERN);
//            } else {
//                formatter = new ExtendedPatternFormatter(pattern);
//            }
//
//            if (fileName == null) {
//                target =
//                    new FileTarget(new File("default.log"), true, formatter);
//            } else {
//                target = new FileTarget(new File(fileName), true, formatter);
//            }
//
//            Hierarchy.getDefaultHierarchy().setDefaultLogTarget(target);
//
//            Logger logger =
//                Hierarchy.getDefaultHierarchy().getLoggerFor("Default");
//
//            if (priority == null) {
//                logger.setPriority(priority);
//            } else {
//                logger.setPriority(Priority.DEBUG);
//            }
//
//            return logger;
//        }
    }
