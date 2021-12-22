### Variables ###
base_url="https://adventofcode.com"
computer_path="/mnt/c/Users/dlabs/Documents/Personal"
extension="hs"
project_path="advent_of_code_2021"

### Command-line arguments ###
year=$1
day=$2

### Parameters ###
# Files
solution_filename="$day.$extension"
test_filename="$day.test.$extension"
input_filename="$day.txt"

# Paths
base_path="$computer_path/$project_path"

solutions_path="$base_path/src"
tests_path="$base_path/tests"
examples_path="$base_path/examples"
inputs_path="$base_path/inputs"
tools_path="$base_path/tools"
cookies_path="$base_path/cookies.txt"

solution_absolute_path="$solutions_path/$solution_filename"
test_absolute_path="$tests_path/$test_filename"
input_absolute_path="$inputs_path/$input_filename"
examples_absolute_path="$base_path/examples/$input_filename"

# HTTP Request
referer="$base_url/$year/day/$day"
input_url="$base_url/$year/day/$day/input"

# Cookies Handling
function cookie() {
  while read line; do
    cookie=$line
  done < $cookies_path

  echo $cookie
}

### Script logic ###
FILES_CREATED=0
FILES_PRESENT=0

echo "-- Generating files for Day $day --"
printf "\n"

# Solution File
echo "+ $project_path/src"
if [ -f "$solution_absolute_path" ]; then
  echo "-> $solution_filename (present)"
  FILES_PRESENT=$(( FILES_PRESENT + 1 ))
else
  sed "s/DAY_NUMBER/$day/" "$tools_path/template.$extension" > $solution_absolute_path
  echo "-> $solution_filename (created)"
  FILES_CREATED=$(( FILES_CREATED + 1 ))
fi
printf "\n"

# Test Files
echo "+ $project_path/tests"

if [ -f "$test_absolute_path" ]; then
  echo "-> $test_filename (present)"
  FILES_PRESENT=$(( FILES_PRESENT + 1 ))
else
  sed "s/DAY_NUMBER/$day/" "$tools_path/template.test.$extension" > $test_absolute_path
  echo "-> $test_filename (created)"
  FILES_CREATED=$(( FILES_CREATED + 1 ))
fi
printf "\n"

# Example File
echo "+ $project_path/examples"

if [ -f "$examples_absolute_path" ]; then
  echo "-> $input_filename (present)"
  FILES_PRESENT=$(( FILES_PRESENT + 1 ))
else
  touch "$examples_path/$input_filename"
  echo "-> $input_filename (created)"
  FILES_CREATED=$(( FILES_CREATED + 1 ))
fi
printf "\n"

# Input File
echo "+ $project_path/inputs"
if [ -f "$input_absolute_path" ]; then
  echo "-> $input_filename (present)"
  FILES_PRESENT=$(( FILES_PRESENT + 1 ))
else
  curl --silent \
      --header "cookie: $(cookie)" \
      --header "referer: $referer" \
      $input_url > $input_absolute_path

  echo "-> $input_filename (created)"
  FILES_CREATED=$(( FILES_CREATED + 1 ))
fi

printf "\n"
printf "%d files created, %d files already present\n" $FILES_CREATED $FILES_PRESENT


