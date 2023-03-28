{
 "cells": [
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "# Creating Accelerometer Cohort\n",
    "\n",
    "### Sources:\n",
    "\n",
    "> https://github.com/OxWearables/rap_wearables/blob/main/1_Extract_Data.ipynb <br />\n",
    " https://www.youtube.com/watch?v=jodNjrYF8po&t=1056s\n",
    "\n",
    "\n",
    "## Cohort Browser Processing\n",
    "\n",
    "The first step in this process was to create the accelerometer cohort. I followed the inclusion criteria outlined by the Oxford Wearables Group, who are leading the charge in terms of analyzing the UK Biobank accelerometer data. Their quality control inclusion criteria do not differ substantially from those elsewhere in the literature. Because the variables required to perform this screening were largely already available in the UK Biobank, I used the Cohort Browser to create the PA Cohort to restrict to only individuals who wore accelerometers and met the inclusion criteria detailed in the flow chart (up to n = 96,660). These criteria include:\n",
    "\n",
    "- Data quality, good calibration IS Yes\n",
    "- Overall acceleration average IS LESS THAN OR EQUAL TO 100\n",
    "- Wear duration overall IS GREATER THAN OR EQUAL TO 3 \n",
    "- Unique hours of wear in a 24 hour cycle (scattered over multiple days) IS GREATER THAN OR EQUAL TO 24\n",
    "\n",
    "This created a cohort (\"PA Cohort\") with 96,660 individuals.\n",
    "\n",
    "\n",
    "## Spark JL Code for PA Cohort, Hospital Inpatient Data, Death Data\n",
    "\n",
    "Through code inspired by the Oxford Wearables group and the YouTube video created by DNANexus, we next ran a Spark JL notebook that created separate datasets for the covariates (PACOHORTCAD.csv), hospital inpatient data (dathes.csv), and death date and cause of death (datdeath.csv and datdeathcause.csv). This separation of datasets allowed for a simpler process to incorporate surgery, hospital inpatient, and death CAD cases.\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Relying on python kernel unless otherwise noted\n",
    "# Import packages\n",
    "import pyspark\n",
    "import dxpy\n",
    "import dxdata\n",
    "import pandas as pd\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Spark Initialization - DO NOT RUN MORE THAN ONCE\n",
    "sc = pyspark.SparkContext()\n",
    "spark = pyspark.sql.SparkSession(sc)\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "\n",
    "# Discover dispensed database name and dataset id\n",
    "dispensed_database_name = dxpy.find_one_data_object(classname = \"database\",\n",
    "    name = \"app*\", folder = \"/\",\n",
    "    name_mode = \"glob\",\n",
    "    describe = True)[\"describe\"][\"name\"]\n",
    "\n",
    "dispensed_dataset_id = dxpy.find_one_data_object(typename = \"Dataset\",\n",
    " name = \"app*.dataset\",\n",
    " folder = \"/\",\n",
    " name_mode = \"glob\")[\"id\"]\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Loading in the dataset\n",
    "dataset = dxdata.load_dataset(id = dispensed_dataset_id)"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Select unit of analysis\n",
    "participant = dataset[\"participant\"]"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Load cohort and make field names vector\n",
    "cohort = dxdata.load_cohort(\"PA Cohort\")\n",
    "\n",
    "\n",
    "field_names = [\"eid\", \"p22001\", \"p31\", \"p191\", \"p21022\", \"p52\", \"p34\", \"p738_i0\", \"p738_i1\", \"p738_i2\", \"p738_i3\",\n",
    "              \"p6138_i0\", \"p6138_i1\", \"p6138_i2\", \"p6138_i3\", \"p20160_i0\", \"p20160_i1\", \"p20160_i2\",\n",
    "              \"p20160_i3\", \"p20116_i0\", \"p20116_i1\", \"p20116_i2\", \"p20116_i3\", \"p3894_i0\", \"p3894_i1\",\n",
    "              \"p3894_i2\", \"p3894_i3\", \"p3627_i0\", \"p3627_i1\", \"p3627_i2\", \"p3627_i3\", \"p4056_i0\", \"p4056_i1\",\n",
    "              \"p4056_i2\", \"p4056_i3\", \"p21001_i0\", \"p21001_i1\", \"p21001_i2\", \"p21001_i3\",\n",
    "              \"p23104_i0\",\"p23104_i1\", \"p23104_i2\", \"p23104_i3\", \"p48_i0\", \"p48_i1\", \"p48_i2\", \"p48_i3\",\n",
    "              \"p49_i0\", \"p49_i1\", \"p49_i2\", \"p49_i3\", \"p40000_i0\", \"p40000_i1\", \"p40007_i0\", \"p40007_i1\",\n",
    "              \"p40001_i0\", \"p40001_i1\", \"p42000\", \"p42006\", \"p131296\", \"p131297\", \"p131298\", \"p131299\",\n",
    "              \"p131300\", \"p131301\", \"p131302\", \"p131303\", \"p131304\", \"p131305\", \"p131306\", \"p131307\",\n",
    "              \"p131342\", \"p131343\", \"p131354\", \"p131355\", \"p131360\", \"p131361\", \"p131362\", \"p131363\",\n",
    "              \"p131364\", \"p131365\", \"p131366\", \"p131367\", \"p131368\", \"p131369\", \"p131378\", \"p131379\",\n",
    "              \"p53_i0\", \"p53_i1\", \"p53_i2\", \"p53_i3\", \"p21003_i0\", \"p21003_i1\", \"p21003_i2\", \"p21003_i3\",\n",
    "      \"p54_i0\", \"p54_i1\", \"p54_i2\", \"p54_i3\", \"p41272\", \"p41282_a0\", \"p41282_a1\", \"p41282_a2\", \"p41282_a3\",\n",
    "              \"p41282_a4\",\"p41282_a5\", \"p41282_a6\", \"p41282_a7\", \"p41282_a8\", \"p41282_a9\", \"p41282_a10\",\n",
    "              \"p41282_a11\", \"p41282_a12\", \"p41282_a13\", \"p41282_a14\",\n",
    "              \"p41282_a15\",\"p41282_a16\", \"p41282_a17\", \"p41282_a18\", \"p41282_a19\", \"p41282_a20\", \"p41282_a21\",\n",
    "              \"p41282_a22\", \"p41282_a23\", \"p41282_a24\", \"p41282_a25\",\n",
    "              \"p41282_a26\",\"p41282_a27\", \"p41282_a28\", \"p41282_a29\", \"p41282_a30\", \"p41282_a31\", \"p41282_a32\",\n",
    "              \"p41282_a33\", \"p41282_a34\", \"p41282_a35\", \"p41282_a36\",\n",
    "              \"p41282_a37\",\"p41282_a38\", \"p41282_a39\", \"p41282_a40\", \"p41282_a41\", \"p41282_a42\", \"p41282_a43\",\n",
    "              \"p41282_a44\", \"p41282_a45\", \"p41282_a46\", \"p41282_a47\",\n",
    "              \"p41282_a48\",\"p41282_a49\", \"p41282_a50\", \"p41282_a51\", \"p41282_a52\", \"p41282_a53\", \"p41282_a54\",\n",
    "              \"p41282_a55\", \"p41282_a56\", \"p41282_a57\", \"p41282_a58\",\n",
    "              \"p41282_a59\",\"p41282_a60\", \"p41282_a61\", \"p41282_a62\", \"p41282_a63\", \"p41282_a64\", \"p41282_a65\",\n",
    "              \"p41282_a66\", \"p41282_a67\", \"p41282_a68\", \"p41282_a69\",\n",
    "              \"p41282_a70\",\"p41282_a71\", \"p41282_a72\", \"p41282_a73\", \"p41282_a74\", \"p41282_a75\", \"p41282_a76\",\n",
    "              \"p41282_a77\", \"p41282_a78\", \"p41282_a79\", \"p41282_a80\",\n",
    "              \"p41282_a81\",\"p41282_a82\", \"p41282_a83\", \"p41282_a84\", \"p41282_a85\", \"p41282_a86\", \"p41282_a87\",\n",
    "              \"p41282_a88\", \"p41282_a89\", \"p41282_a90\", \"p41282_a91\",\n",
    "              \"p41282_a92\",\"p41282_a93\", \"p41282_a94\", \"p41282_a95\", \"p41282_a96\", \"p41282_a97\", \"p41282_a98\",\n",
    "              \"p41282_a99\", \"p41282_a100\", \"p41282_a101\", \"p41282_a102\",\n",
    "              \"p41282_a103\",\"p41282_a104\", \"p41282_a105\", \"p41282_a106\", \"p41282_a107\", \"p41282_a108\", \"p41282_a109\",\n",
    "              \"p41282_a110\", \"p41282_a111\", \"p41282_a112\", \"p41282_a113\",\n",
    "              \"p41282_a114\",\"p41282_a115\", \"p41282_a116\", \"p41282_a117\", \"p41282_a118\", \"p41282_a119\", \"p41282_a120\",\n",
    "              \"p41282_a121\", \"p41282_a122\", \"p41282_a123\", \"p41283_a0\", \"p41283_a1\", \"p41283_a2\", \"p41283_a3\",\n",
    "              \"p41283_a4\",\"p41283_a5\", \"p41283_a6\", \"p41283_a7\", \"p41283_a8\", \"p41283_a9\", \"p41283_a10\",\n",
    "              \"p41283_a11\", \"p41283_a12\", \"p41283_a13\", \"p41283_a14\",\n",
    "              \"p41283_a15\",\"p22021\", \"p90012\", \"p90013\", \"p90027\", \"p90028\", \"p90029\", \"p90030\",\n",
    "      \"p90031\", \"p90032\", \"p90033\", \"p90034\", \"p90035\", \"p90036\", \"p90037\", \"p90038\", \"p90039\", \"p90040\",\n",
    "      \"p90041\", \"p90042\", \"p90043\", \"p90044\", \"p90045\", \"p90046\", \"p90047\", \"p90048\", \"p90049\", \"p90050\",\n",
    "      \"p90019\", \"p90020\", \"p90021\", \"p90022\", \"p90023\", \"p90024\", \"p90025\", \"p90087\", \"p90088\", \"p90091\", \"p90090\", \"p90089\",\n",
    "      \"p90092\", \"p90093\", \"p90094\", \"p90095\", \"p90096\", \"p90097\", \"p90098\", \"p90099\", \"p90100\", \"p90101\",\n",
    "      \"p90102\", \"p90103\", \"p90104\", \"p90105\", \"p90106\", \"p90107\", \"p90108\", \"p90109\", \"p90110\", \"p90111\",\n",
    "      \"p90112\", \"p90113\", \"p90114\", \"p90115\", \"p90116\", \"p90117\", \"p90118\", \"p90119\", \"p90120\", \"p90121\",\n",
    "      \"p90122\", \"p90123\", \"p90124\", \"p90125\", \"p90126\", \"p90127\", \"p90128\", \"p90129\", \"p90130\", \"p90131\",\n",
    "      \"p90132\", \"p90133\", \"p90134\", \"p90135\", \"p90136\", \"p90137\", \"p90138\", \"p90139\", \"p90140\", \"p90141\",\n",
    "      \"p90142\", \"p90143\", \"p90144\", \"p90145\", \"p90146\", \"p90147\", \"p90148\", \"p90149\", \"p90150\", \"p90151\",\n",
    "      \"p90152\", \"p90153\", \"p90154\", \"p90155\", \"p90156\", \"p90157\", \"p90158\", \"p90015\", \"p90003\", \"p90010\",\n",
    "      \"p90011\", \"p90026\", \"p90018\", \"p90084\", \"p90085\", \"p90051\", \"p90052\", \"p90086\", \"p90060\", \"p90061\",\n",
    "      \"p90062\", \"p90063\", \"p90064\", \"p90065\", \"p90066\", \"p90067\", \"p90068\", \"p90069\", \"p90070\", \"p90071\",\n",
    "      \"p90072\", \"p90073\", \"p90074\", \"p90075\", \"p90076\", \"p90077\", \"p90078\", \"p90079\", \"p90080\", \"p90081\",\n",
    "      \"p90082\", \"p90083\", \"p90057\", \"p90053\", \"p90058\", \"p90059\", \"p90056\", \"p90054\", \"p90055\", \"p90016\",\n",
    "      \"p90017\", \"p90159\", \"p90160\", \"p90170\", \"p90171\", \"p90173\", \"p90175\", \"p90177\", \"p90172\", \"p90174\",\n",
    "      \"p90176\", \"p90161\", \"p90164\", \"p90167\", \"p90162\", \"p90165\", \"p90168\", \"p90163\", \"p90166\", \"p90169\",\n",
    "      \"p90002\", \"p90182\", \"p90179\", \"p90187\", \"p90188\", \"p90189\", \"p90190\", \"p90191\", \"p90180\", \"p90181\",\n",
    "      \"p90183\", \"p90185\", \"p90184\", \"p90186\", \"p90192\", \"p90193\", \"p90194\", \"p90195\", \"p90004\", \"p22006\",\n",
    "               \"p22418\", \"p190\", \"p22019\", \"p22027\"]\n",
    "\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Subset cohort to only relevant field names\n",
    "df = participant.retrieve_fields(names = field_names,\n",
    " filter_sql = cohort.sql,\n",
    " coding_values = \"replace\",\n",
    " engine = dxdata.connect())\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Convert to pandas dataframe\n",
    "df_pandas = df.toPandas()\n",
    "df_pandas.head()\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Write csv\n",
    "df_pandas.to_csv(\"PACOHORTCAD.csv\")\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Adding hospital inpatient data as separate datasets\n",
    "\n",
    "hesin = dataset[\"hesin\"]\n",
    "hesin_diag = dataset[\"hesin_diag\"]\n",
    "\n",
    "hesin_data = hesin.retrieve_fields(engine=dxdata.connect())\n",
    "hesin_diag_data = hesin_diag.retrieve_fields(engine=dxdata.connect())\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Imputing if missing start date\n",
    "hesin_data = hesin_data.withColumn(\"dateepiimp\",\n",
    "                                   when(hesin_data[\"epistart\"].isNotNull(), hesin_data[\"epistart\"]).otherwise(hesin_data[\"disdate\"]))\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Merging diagnosis and date variables for hospital inpatient records\n",
    "hes_data = hesin_data.join(\n",
    "    hesin_diag_data, \n",
    "    [\"eid\", \"ins_index\", \"dnx_hesin_id\"],\n",
    "    \"left_outer\"\n",
    ")\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Creating csv file for participant data\n",
    "hes_data.coalesce(1).write.mode(\"overwrite\").option(\"header\", \"true\").csv(\"hes_data\")\n",
    "\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Uploading hospital inpatient records\n",
    "%%bash\n",
    "dx upload hes_data/*.csv \n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Adding death register data as separate datasets\n",
    "death = dataset[\"death\"]\n",
    "death_cause = dataset[\"death_cause\"]\n",
    "\n",
    "death_data = death.retrieve_fields(engine=dxdata.connect())\n",
    "death_cause_data = death_cause.retrieve_fields(engine=dxdata.connect())\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Writing death data and causes of death as separate csvs\n",
    "death_data.coalesce(1).write.mode(\"overwrite\").option(\"header\", \"true\").csv(\"death_data\")\n",
    "death_cause_data.coalesce(1).write.mode(\"overwrite\").option(\"header\", \"true\").csv(\"death_cause_data\")\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Uploading these csvs\n",
    "%%bash\n",
    "dx upload death_data/*.csv \n",
    "dx upload death_cause_data/*.csv \n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Uploading PA Cohort csv separately\n",
    "%%bash\n",
    "dx upload \"PACOHORTCAD.csv\"\n"
   ]
  },
  {
   "cell_type": "markdown",
   "metadata": {},
   "source": [
    "The above process yielded datasets:\n",
    "- PACOHORTCAD.csv (Overall phenotypic dataset for individuals meeting accelerometer inclusion criteria)\n",
    "- dathes.csv (Hospital inpatient episode records and dates associated with episodes)\n",
    "- datdeath.csv (Death data with dates associated with deaths)\n",
    "- datdeathcause.csv (Cause of death date)\n",
    "\n",
    "## Hours Worn Conversion\n",
    "\n",
    "Lastly, we created a short R file to simply convert days worn to hours worn and created “PACOHORTprocessedPAVars.csv.”"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Start as Bash kernel\n",
    "dx download PACOHORT.csv\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Changed to R kernel\n",
    "PACOHORT <- read.csv(\"PACOHORT.csv\")\n",
    "\n",
    "summary(PACOHORT$p90051)\n",
    "# Wear time (IN DAYS)\n",
    "\n",
    "# Converting to wear time in hours\n",
    "PACOHORT$HoursWorn <- PACOHORT$p90051*24\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Writing to csv\n",
    "write.csv(PACOHORT, \"PACOHORTprocessedPAVars.csv\")\n"
   ]
  },
  {
   "cell_type": "code",
   "execution_count": null,
   "metadata": {},
   "outputs": [],
   "source": [
    "# Changed to Bash kernel\n",
    "dx upload PACOHORTprocessedPAVars.csv"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.8.5"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 4
}
