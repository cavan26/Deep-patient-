###############################################################################
# (Script)
# Loads dumps from the STRIDE database and processes them according to the
# rules stated in the deep patient paper.
# cf README for more info.
#
# Nathanael Romano
###############################################################################

rm(list=ls())
library("dplyr")

# Path so save the data and metadata
REPO_PATH = '/Users/camille.vanassel/Data Intelligence/Deep Patient '
PATH = ''
METADATA_PATH = paste(REPO_PATH, 'data', sep='/')

# Load data
codes = as.data.frame(
  read.csv(paste(METADATA_PATH, 'Medcode.csv', sep='/'))
)
patients = as.data.frame(
  read.csv(paste(METADATA_PATH, 'Patients.csv', sep='/'))
)
orders = as.data.frame(
  read.csv(paste(METADATA_PATH, 'Orders.csv', sep='/'))
)

# Fetch descriptors
all.codes = unique(codes$medcode)
all.patients = unique(codes$patid)

codes = mutate(codes, eventdate= as.Date(eventdate, format= "%Y-%m-%d"))
codes$year <- format(codes$eventdate, "%Y")
orders.2014 = filter(orders, start_time_year >= 2014)
orders = filter(orders, start_time_year < 2014)

# Number of events
nb_events = length(codes$patid)
  
# Compute the age at event date, in days
nb_of_event_per_patient = rle(as.numeric(codes$patid))
nb_of_event_per_patient_cumsum = cumsum(nb_of_event_per_patient$lengths)
age_at_visit = list()
for (x in c(1:nb_events))
  age_at_visit[x] <- codes$eventdate[x] - as.Date(as.character(patients$yob[min(which(nb_of_event_per_patient_cumsum >= x))]), format="%Y-%m-%d")
codes$age_at_visit_in_days = as.numeric(age_at_visit)

# Filter years according to cutting point.
codes.2014 = filter(codes, year >= 2014)
codes = filter(codes, year < 2014)

# Count codes before and after splitting point.
by_code <- group_by(codes.2014, patid, medcode)
codes.2014 <- as.data.frame(summarize(
  by_code, 
  count=n(),
  age=mean(age_at_visit_in_days, na.rm=T)
))
by_code <- group_by(codes, patid, medcode)
codes <- as.data.frame(summarize(
  by_code, 
  count=n(), 
  age=mean(age_at_visit_in_days, na.rm=T)
))
by_code <- group_by(orders.2014, patid, ahdid)
orders.2014 <- as.data.frame(summarize(
  by_code, 
  count=n(), 
  age=mean(age_at_start_time_in_days, na.rm=T)
))
by_code <- group_by(orders, patid, ahdid)
orders <- as.data.frame(summarize(
  by_code, 
  count=n(), 
  age=mean(age_at_start_time_in_days, na.rm=T)
))

# Remove patients with too few records before 2014
ids <- unique(codes$patid)
totalCounts <- data.frame(count=rep(0, length(ids)))
rownames(totalCounts) <- ids

by_patient <- group_by(orders, patid)
counts <- as.data.frame(summarize(by_patient, count=n()))
totalCounts[as.character(counts$patid), "count"] = 
  totalCounts[as.character(counts$patid), "count"] + counts[, "count"]

by_patient <- group_by(codes, patid)
counts <- as.data.frame(summarize(by_patient, count=n()))
totalCounts[as.character(counts$patid), "count"] = 
  totalCounts[as.character(counts$patid), "count"] + counts[, "count"]

# Patient IDs to keep
ids <- ids[totalCounts$count >= 5]
codes.2014 <- filter(codes.2014, patid %in% ids, !is.null(medcode))

# Remove codes appearing in less than 5 patients or more than 80% of patients.
by_code <- group_by(codes, medcode)
counts <- as.data.frame(summarize(by_code, count=n()))
codes.to.keep <- filter(counts, count >= 5, 
                        count < 0.8 * length(unique(codes$patid)))$medcode

by_code <- group_by(orders, ahdid)
counts <- as.data.frame(summarize(by_code, count=n()))
orders.to.keep <- filter(counts, count >= 5, 
                         count < 0.8 * length(unique(orders$patid)))$ahdid

# Sample test patients, and hold them back from training set
test.patients = unique(codes.2014$patid)
test.patients = test.patients[rownames(filter(totalCounts, count >= 10))]
test.patients = base::sample(unique(codes.2014$patid), size=4000)

# Add test patients for downstream task
test.demo = filter(patients, patid %in% test.patients)
test.codes = filter(codes, patid %in% test.patients)
test.orders = filter(orders, patid %in% test.patients)
test.targets = filter(codes.2014, patid %in% test.patients,
                      medcode %in% codes.to.keep)

# Hold back test patients from source task training set
codes = filter(codes, patid %in% ids, !is.null(medcode),
               medcode %in% codes.to.keep,
               !(patid %in% test.patients))
demo = filter(demo, patient_id %in% ids, 
              !(patient_id %in% test.patients))

# Choose training patients for downstream task
train.patients = base::sample(unique(codes$patid), size=200000)
train.demo = filter(demo, patid %in% train.patients)
train.codes = filter(codes, patid %in% train.patients)
train.orders = filter(orders, patid %in% train.patients)
train.targets = filter(codes.2014, patid %in% train.patients)

# Save data
# Training data
write.table(codes,
            file=paste(PATH, 'codes.csv', sep='/'),
            row.names=F)
write.table(orders, 
            file=paste(PATH, 'orders.csv', sep='/'),
            row.name=F)
write.table(demo, 
            file=paste(PATH, 'demo.csv', sep='/'),
            row.name=F)
# Disease prediction traininig data
write.table(train.demo,
            file=paste(PATH, 'train.demo.csv', sep='/'),
            row.names=F)
write.table(train.codes,
            file=paste(PATH, 'train.codes.csv', sep='/'),
            row.names=F)
write.table(train.orders,
            file=paste(PATH, 'train.orders.csv', sep='/'),
            row.names=F)
write.table(train.targets,
            file=paste(PATH, 'train.targets.csv', sep='/'),
            row.names=F)
# Disease prediction validation and test data
write.table(test.demo,
            file=paste(PATH, 'test.demo.csv', sep='/'),
            row.names=F)
write.table(test.codes,
            file=paste(PATH, 'test.codes.csv', sep='/'),
            row.names=F)
write.table(test.orders,
            file=paste(PATH, 'test.orders.csv', sep='/'),
            row.names=F)
write.table(test.targets,
            file=paste(PATH, 'test.targets.csv', sep='/'),
            row.names=F)

# Metadata
write(all.codes, file=paste(METADATA_PATH, 'codes.txt', sep='/'), sep='\n')
write(all.orders, file=paste(METADATA_PATH, 'orders.txt', sep='/'), sep=', ')
write(ids, file=paste(METADATA_PATH, 'patients.txt', sep='/'), sep='\n')
write(train.patients,
      file=paste(METADATA_PATH, 'train.patients.txt', sep='/'), sep='\n')
write(test.patients,
      file=paste(METADATA_PATH, 'test.patients.txt', sep='/'), sep='\n')
