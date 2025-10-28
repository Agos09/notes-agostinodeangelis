import * as React from "react";

interface SearchInputProps {
  placeholder?: string;
}

export function SearchInput({ placeholder = "Search" }: SearchInputProps) {
  return (
    <div className="mt-10 w-80 max-w-full text-base text-gray-500 whitespace-nowrap">
      <div className="w-full">
        <div className="w-full">
          <div className="flex overflow-hidden gap-2 items-center px-4 py-3 w-full bg-white rounded-lg border border-gray-300 border-solid shadow-sm">
            <div className="flex flex-1 shrink gap-2 items-center self-stretch my-auto w-full basis-0 min-w-60">
              <img
                src="https://api.builder.io/api/v1/image/assets/TEMP/f51d8dc1392ec07e19f3f360c52e8612929afdba?placeholderIfAbsent=true&apiKey=c00da45f4ed64d9893425a2094d3e1af"
                className="object-contain shrink-0 self-stretch my-auto w-5 aspect-square"
                alt="Search icon"
              />
              <div className="flex-1 shrink self-stretch my-auto text-gray-500 basis-0">
                {placeholder}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
