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
 * @(#)RandomEventGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.util.IOUtil;
import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.List;
import java.util.ArrayList;
import java.util.Random;

/**
 * RandomEventGenerator.java
 *
 * Created on June 2, 2006, 11:35 AM
 *
 * @author Rahul Dwivedi
 */
public class RandomEventGenerator {
    private static final Messages mMessages = Messages.getMessages(RandomEventGenerator.class);

    private List<List<RandomData>> mRowList;
    private String[] mColumnNames;
    private static int INTTYPE = 0;
    private static int FLOATTYPE = 1;

    public RandomEventGenerator(String eventDescriptorFile) {
        mRowList = intialize(eventDescriptorFile);
    }

    private List<List<RandomData>> intialize(String eventDescriptorFile) {
        List<List<RandomData>> rowList = new ArrayList<List<RandomData>>();
        BufferedReader fileIn = null;
        int colCnt = 0;
        try {
            fileIn = new BufferedReader(new InputStreamReader(IOUtil.getResourceAsStream(eventDescriptorFile)));
            String record = fileIn.readLine();
            if (record == null) {
                return rowList;
            }
            while (record != null && record.startsWith("#")) {
                record = fileIn.readLine(); // skip comments
            }
            if (record == null) {
                return rowList;
            }
            StringTokenizer st = new StringTokenizer(record, ",");
            colCnt = st.countTokens();
            mColumnNames = new String[colCnt];
            for (int i = 0; i < colCnt; i++) {
                mColumnNames[i] = st.nextToken();
            }
            while (true) {
                record = fileIn.readLine();
                if (record == null) {
                    break;
                }
                if (record.startsWith("#")) {
                    continue;  // skip comments
                }
                List<RandomData> row = new ArrayList<RandomData>();
                st = new StringTokenizer(record, ",");


                for (int i = 0; i < colCnt; i++) {
                    if (st.hasMoreTokens()) {
                        String strPattern = (String) st.nextToken();

                        if (strPattern.startsWith("S")) {
                            row.add(new StringData(strPattern));
                        } else if (strPattern.startsWith("F")) {
                            row.add(new FloatData(strPattern));
                        } else if (strPattern.startsWith("B")) {
                            row.add(new RandomBoolean(strPattern));
                        } else if (strPattern.startsWith("I")) {
                            row.add(new IntegerData(strPattern));
                        } else if (strPattern.startsWith("C")) {
                            row.add(new StringData(1, strPattern));
                        }
                    }
                }
                rowList.add(row);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "RandomEventGenerator.loadData_fails", e);
        } finally {
            try {
                if (fileIn != null) {
                    fileIn.close();
                }
            } catch (Exception e1) {
                mMessages.log(Level.SEVERE, "RandomEventGenerator.Closing_fileIn_fails", eventDescriptorFile, e1);
            }
        }
        return rowList;
    }

    public List<List<RandomData>> getRowDescriptionList() {
        return mRowList;
    }

    static class StringData implements RandomData {
        //S
        //S{a,b,c}
        private int mLenght = -1;
        private String mPattern = null;
        boolean mFixedArray = false;
        //boolean mFixedLength = false;
        private List<Object> mSubSetElements = new ArrayList<Object>();
        private Random rand = new Random();

        public StringData(String pat) {
            mPattern = pat;
            parseRule();
        }

        public StringData(int length, String pat) {
            mLenght = length;
            mPattern = pat;
            parseRule();
        }

        public Object getData() {
            if (mFixedArray) {
                Object obj = mSubSetElements.get(rand.nextInt(mSubSetElements.size()));
                if (obj instanceof RandomData) {
                    return ((RandomData) obj).getData();
                } else {
                    return obj;
                }

            } else {
                String data = "D" + Math.random();
                if (mLenght < 1) {
                    return data;
                } else {
                    return data.charAt(data.length() - 1);
                }
            }
        }

        private void parseRule() {
            mFixedArray = mPattern.charAt(1) == '{';
            if (mFixedArray) {
                String subSet = mPattern.substring(2, mPattern.indexOf('}'));
                StringTokenizer token = new StringTokenizer(subSet, ";");
                while (token.hasMoreElements()) {
                    mSubSetElements.add(token.nextElement());
                }
            }
        }
    }

    static class IntegerData implements RandomData {
        // I
        // I{1;2;3}   or {1...3}
        // IC123  where 123 is the start index. default is 0;
        private Random rand = new Random();
        private String mPattern = null;
        boolean mFixedArray = false;
        boolean isContinous = false;
        int start = 0;
        private List<Object> mSubSetElements = new ArrayList<Object>();

        public IntegerData(String pat) {
            mPattern = pat;
            parseRule();
        }

        public Object getData() {
            if (mFixedArray) {

                Object obj = mSubSetElements.get(rand.nextInt(mSubSetElements.size()));
                if (obj instanceof RandomData) {
                    return ((RandomData) obj).getData();
                } else {
                    return obj;
                }
            } else if (isContinous) {
                return new Integer(start++);
            } else {
                return new Integer(rand.nextInt(1000));
            }
        }

        private void parseRule() {
            if (mPattern.length() > 1) {
                mFixedArray = mPattern.charAt(1) == '{';
                isContinous = mPattern.charAt(1) == 'C';

                if (mFixedArray) {
                    String subSet = mPattern.substring(2, mPattern.indexOf('}'));
                    StringTokenizer token = new StringTokenizer(subSet, ";");

                    while (token.hasMoreElements()) {
                        String str = (String) token.nextElement();
                        try {
                            if (str.indexOf("...") > 0) {
                                //Discovered Range
                                mSubSetElements.add(new RandomWithinRange(str, INTTYPE));
                            } else {
                                Integer val = Integer.valueOf(str);
                                mSubSetElements.add(val);
                            }
                        } catch (Exception e) {
                            mMessages.log(Level.SEVERE, "RandomEventGenerator.Unable_to_parse_to_integer_ignoring", str, e);
                        }

                    }
                } else if (isContinous) {
                    if (mPattern.length() > 2) {
                        String startIndex = mPattern.substring(2);
                        try {
                            start = Integer.parseInt(startIndex);
                        } catch (Exception e) {
                        }
                    }
                }
            }
        }
    }

    static class FloatData implements RandomData {

        private String mPattern = null;
        private Random rand = new Random();
        private boolean mFixedArray = false;
        private List<Object> mSubSetElements = new ArrayList<Object>();

        public FloatData(String pat) {
            mPattern = pat;
            parseRule();

        }

        private void parseRule() {
            if (mPattern.length() > 1) {
                mFixedArray = mPattern.charAt(1) == '{';
                if (mFixedArray) {
                    String subSet = mPattern.substring(2, mPattern.indexOf('}'));
                    StringTokenizer token = new StringTokenizer(subSet, ";");
                    while (token.hasMoreElements()) {
                        String str = (String) token.nextElement();
                        try {
                            if (str.indexOf("...") > 0) {
                                //Discovered Range
                                mSubSetElements.add(new RandomWithinRange(str, FLOATTYPE));
                            } else {
                                Float val = Float.valueOf(str);
                                mSubSetElements.add(val);
                            }
                        } catch (Exception e) {
                            mMessages.log(Level.SEVERE, "RandomEventGenerator.Unable_to_parse_to_Float_ignoring", str, e);
                        }

                    }
                }
            }
        }

        public Object getData() {
            if (mFixedArray) {
                Object obj = mSubSetElements.get(rand.nextInt(mSubSetElements.size()));
                if (obj instanceof RandomData) {
                    return ((RandomData) obj).getData();
                } else {
                    return obj;
                }
            } else {
                return new Float(rand.nextFloat());
            }

        }
    }

    static class RandomBoolean implements RandomData {

        private Random rand = new Random();
        //private String strPattern = null;
        private Boolean mLs[] = {Boolean.TRUE, Boolean.FALSE};

        public RandomBoolean(String pat) {
        //strPattern = pat;
        }

        public Object getData() {
            return mLs[rand.nextInt(2)];
        }
    }

    static class RandomWithinRange implements RandomData {

        private Random mRand = new Random();
        //private String mPattern = null;
        private int mType = 0;
        private Object mFPoint = null;
        private Object mLPoint = null;

        public RandomWithinRange(String pattern, int type) throws Exception {
            mType = type;
            //string split dosen;t work since "..." is recognized as any char pattern in reg  expression.
            // ToDo Need better way to get two points for the range. 
            StringTokenizer tokenizer = new StringTokenizer(pattern, "...");
            String[] range = new String[tokenizer.countTokens()];
            int index = 0;
            while (tokenizer.hasMoreElements()) {
                range[index++] = (String) tokenizer.nextElement();
            }

            if (range.length == 2) {

                if (type == INTTYPE) {
                    mFPoint = Integer.valueOf(range[0]);
                    mLPoint = Integer.valueOf(range[1]);
                    mRand.setSeed(Integer.parseInt(range[0]));

                } else if (type == FLOATTYPE) {
                    mFPoint = Float.valueOf(range[0]);
                    mLPoint = Float.valueOf(range[1]);
                    float f = Float.parseFloat(range[0]);
                    mRand.setSeed(Math.round(f));
                }
            } else {
                throw new Exception(mMessages.getString("RandomEventGenerator.Invalid_Range_Type"));
            }
        }

        public Object getData() {
            if (mType == INTTYPE) {
                return new Integer(mRand.nextInt(((Integer) mLPoint).intValue()));
            } else if (mType == FLOATTYPE) {
                float f = mRand.nextFloat();
                return new Float(((Float) mFPoint).floatValue() * f + ((Float) mLPoint).floatValue() * (1 - f));
            } else {
                return null;
            }
        }
    }
}
